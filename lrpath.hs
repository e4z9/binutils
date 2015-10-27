module Main where

  import Control.Concurrent.Async
  import Control.Monad
  import Data.List
  import Data.Map.Strict (Map)
  import qualified Data.Map.Strict as Map
  import System.Directory
  import System.Environment
  import System.FilePath (pathSeparator)
  import System.IO.Unsafe
  import System.Process

  otoolLoadCommandOutput :: FilePath -> IO String
  otoolLoadCommandOutput filePath = do
    (_, out, _) <- readProcessWithExitCode "otool" ["-l", filePath] ""
    return out

  -- splitting up into sections
  -- section data type
  data SectionType = LoadCommandSection | OtherSection deriving (Show)
  data Section = Section SectionType (Map String String)

  instance Show Section where
    show (Section t d) = "Section: " ++ show t ++ " " ++ show d

  emptySection :: SectionType -> Section
  emptySection t = Section t Map.empty

  insertSectionData :: (String, String) -> Section -> Section
  insertSectionData ([], _) section = section
  insertSectionData (key, value) (Section t d) = Section t (Map.insert key value d)

  --
  parseSectionType :: String -> Maybe SectionType
  parseSectionType s
    | "Load command" `isPrefixOf` s = Just LoadCommandSection
    | "Section" `isPrefixOf` s = Just OtherSection
    | otherwise = Nothing

  -- parse a key-value pair from  aline of section details from otool output
  -- using 'words' is somewhat of a poor-mans implementation, but sufficient for the use cases
  parseSectionData :: String -> (String, String)
  parseSectionData s = case words s of
    [key, value] -> (key, value)
    ("path":path:_) -> ("path", path)
    ("name":path:_) -> ("name", path)
    _ -> ("", "")

  -- output line -> (accumulatedSections, currentSection) -> newAcc
  splitSectionsHelper :: ([Section], Maybe Section) -> String -> ([Section], Maybe Section)
  splitSectionsHelper (sections, Nothing) s = case parseSectionType s of
      Just sectionType -> (sections, Just (emptySection sectionType))
      Nothing -> (sections, Nothing)
  splitSectionsHelper (sections, Just currentSection) s = case parseSectionType s of
      Just sectionType -> (currentSection:sections, Just (emptySection sectionType))
      Nothing -> (sections, Just (insertSectionData (parseSectionData s) currentSection))

  splitSections :: String -> [Section]
  splitSections l = case foldl' splitSectionsHelper ([], Nothing) (lines l) of
                      (reverseSections, Nothing) -> reverse reverseSections
                      (reverseSections, Just lastSection) -> reverse (lastSection:reverseSections)

  rpathOfSection :: Section -> Maybe String
  rpathOfSection (Section LoadCommandSection d) = case (Map.lookup "cmd" d, Map.lookup "path" d) of
    (Just "LC_RPATH", Just path) -> Just path
    _ -> Nothing
  rpathOfSection _ = Nothing

-- parse rpaths from otool sections
  rpathsFromOtoolSections :: [Section] -> Maybe [String]
  rpathsFromOtoolSections [] = Nothing
  rpathsFromOtoolSections sections = Just (foldr addRpath [] sections)
    where addRpath s acc = case rpathOfSection s of Just rpath -> rpath:acc; _ -> acc

-- get all rpaths for file path
  readRpaths :: FilePath -> IO (Maybe [String])
  readRpaths filePath = do
    otoolOutput <- otoolLoadCommandOutput filePath
    let rpaths = rpathsFromOtoolSections $ splitSections otoolOutput
    return rpaths

-- parMapSeqFinalizeIO
  finalizeHelper :: (b -> IO ()) -> Async b -> IO ()
  finalizeHelper f ax = do
      x <- wait ax
      f x

  -- mapFun -> finalizeFun -> mapQueue -> finalizeQueue -> nothing
  parMapSeqFinalizeIOHelper :: (a -> IO b) -> Int -> (b -> IO ()) -> [a] -> [Async b] -> IO ()
  parMapSeqFinalizeIOHelper _ _ _ [] [] = return ()
  parMapSeqFinalizeIOHelper m c f [] (a:as) = do
      finalizeHelper f a
      parMapSeqFinalizeIOHelper m c f [] as
  parMapSeqFinalizeIOHelper m c f (x:xs) [] = do
      newA <- async (m x)
      parMapSeqFinalizeIOHelper m c f xs [newA]
  parMapSeqFinalizeIOHelper m c f xAll@(x:xs) aAll@(a:as)
    | length aAll >= c = do
        finalizeHelper f a
        parMapSeqFinalizeIOHelper m c f xAll as
    | otherwise = do
        newA <- async (m x)
        parMapSeqFinalizeIOHelper m c f xs (aAll ++ [newA])

  -- mapFun -> capabilities -> finalizeFun -> list -> nothing
  parMapSeqFinalizeIO :: (a -> IO b) -> Int -> (b -> IO ()) -> [a] -> IO ()
  parMapSeqFinalizeIO m f c xs = parMapSeqFinalizeIOHelper m f c xs []

-- lazily recurse through directories
  lazyRecursiveFilePathsHelper :: [FilePath] -> IO [FilePath]
  lazyRecursiveFilePathsHelper [] = return []
  lazyRecursiveFilePathsHelper (filePath:l) = do
      isFile <- doesFileExist filePath
      isDirectory <- doesDirectoryExist filePath
      case (isFile, isDirectory) of
        (True, False) -> do
            morePaths <- unsafeInterleaveIO $ lazyRecursiveFilePathsHelper l
            return (filePath:morePaths)
        (False, True) -> do
            files <- getDirectoryContents filePath
            let filePaths = [filePath ++ [pathSeparator] ++ file | file <- files, file `notElem` [".", ".."]]
            lazyRecursiveFilePathsHelper (filePaths ++ l)
        _ -> lazyRecursiveFilePathsHelper l

  lazyRecursiveFilePaths :: FilePath -> IO [FilePath]
  lazyRecursiveFilePaths filePath = unsafeInterleaveIO $ lazyRecursiveFilePathsHelper [filePath]

  -- print all rpaths in hierarchy
  putIndentedList :: String -> [String] -> IO ()
  putIndentedList _ [] = return ()
  putIndentedList indent (l:ls) = do
    putStrLn $ indent ++ l
    putIndentedList indent ls

  mapRpaths :: FilePath -> IO (FilePath, Maybe [String])
  mapRpaths filePath = do
      rpaths <- readRpaths filePath
      return (filePath, rpaths)

  printRpaths :: (FilePath, Maybe [String]) -> IO ()
  printRpaths (_, Nothing) = return ()
  printRpaths (filePath, Just rpaths) = do
      putStr filePath
      putStr ":"
      when (null rpaths) $ putStr " None"
      putStrLn ""
      putIndentedList "    " rpaths

  main :: IO ()
  main = do
    args <- getArgs
    filePaths <- lazyRecursiveFilePaths $ head args
    parMapSeqFinalizeIO mapRpaths 8 printRpaths filePaths
