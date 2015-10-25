module Main where

  import Control.Monad
  import Data.List
  import Data.Map.Strict (Map)
  import qualified Data.Map.Strict as Map
  import System.Directory
  import System.Environment
  import System.FilePath (pathSeparator)
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

-- parse rpaths from otool output
  rpathsFromOtool :: String -> [String]
  rpathsFromOtool otoolOutput = foldr addRpath [] (splitSections otoolOutput)
    where addRpath s acc = case rpathOfSection s of Just rpath -> rpath:acc; _ -> acc

-- get all rpaths for file path
  readRpaths :: FilePath -> IO [String]
  readRpaths filePath = do
    otoolOutput <- otoolLoadCommandOutput filePath
    let rpaths = rpathsFromOtool otoolOutput
    return rpaths

-- get multiple rpaths
  recursiveRpathsHelper :: [FilePath] -> Map String [String] -> IO (Map String [String])
  recursiveRpathsHelper [] acc = return acc
  recursiveRpathsHelper (filePath:l) acc = do
        isFile <- doesFileExist filePath
        isDirectory <- doesDirectoryExist filePath
        case (isFile, isDirectory) of
          (True, False) -> do
              rpaths <- readRpaths filePath
              recursiveRpathsHelper l (Map.insert filePath rpaths acc)
          (False, True) -> do
              files <- getDirectoryContents filePath
              let filePaths = [filePath ++ [pathSeparator] ++ file | file <- files, file `notElem` [".", ".."]]
              recursiveRpathsHelper (filePaths ++ l) acc
          _ -> recursiveRpathsHelper l acc

  recursiveRpaths :: FilePath -> IO (Map String [String])
  recursiveRpaths filePath = recursiveRpathsHelper [filePath] Map.empty

  putIndentedList :: String -> [String] -> IO ()
  putIndentedList _ [] = return ()
  putIndentedList indent (l:ls) = do
    putStrLn $ indent ++ l
    putIndentedList indent ls

  putRpathList :: [(String, [String])] -> IO ()
  putRpathList [] = return ()
  putRpathList ((filePath, rpaths):l) = do
    unless (null rpaths) $ do
      putStr filePath
      putStrLn ":"
      putIndentedList "    " rpaths
    putRpathList l

  putRpathMap :: Map String [String] -> IO ()
  putRpathMap m = putRpathList $ Map.toAscList m

  main :: IO ()
  main = do
    args <- getArgs
    rpaths <- recursiveRpaths $ head args
    putRpathMap rpaths
