-- |
-- Module      :  Ez.System.Otool
-- Copyright   :  (c) Eike Ziller
-- License     :  MIT
--
-- Stability   :  experimental
-- Portability :  non-portable (OS X - only)
--
-- Running @otool@ on files and interpreting its output.
module Ez.System.Otool (readRpaths) where

  import Data.List
  import Data.Map.Strict (Map)
  import qualified Data.Map.Strict as Map
  import System.Process

  -- | Runs @otool -l@ on the given file path and returns its output.
  otoolLoadCommandOutput :: FilePath -> IO String
  otoolLoadCommandOutput filePath = do
    (_, out, _) <- readProcessWithExitCode "otool" ["-l", filePath] ""
    return out

  -- | Type of a Mach-O load command section.
  data SectionType = LoadCommandSection -- ^ Load command.
      | OtherSection -- ^ Other section.
      deriving (Show)

  -- | One Mach-O load command section with key-value pairs.
  data Section = Section SectionType (Map String String) -- ^ Creates a section with given type and data.

  -- | Shows 'Section' type and key-value map.
  instance Show Section where
    show (Section t d) = "Section: " ++ show t ++ " " ++ show d

  -- | Creates a new, empty 'Section' of the given type.
  emptySection :: SectionType -> Section
  emptySection t = Section t Map.empty

  -- | Adds the given (key, value) pair to the 'Section'.
  insertSectionData :: (String, String) -> Section -> Section
  insertSectionData ([], _) section = section
  insertSectionData (key, value) (Section t d) = Section t (Map.insert key value d)

  -- | Returns 'Just' the section type, if the input line corresponds
  --   to a section header from @otool@ output, or 'Nothing' otherwise.
  parseSectionType :: String -> Maybe SectionType
  parseSectionType s
    | "Load command" `isPrefixOf` s = Just LoadCommandSection
    | "Section" `isPrefixOf` s = Just OtherSection
    | otherwise = Nothing

  -- | Parses a key-value pair from a line of section details from @otool@ output.
  --   It currently uses 'words' to split key and value, which is somewhat of a
  --   poor-mans implementation, but was sufficient for the use cases so far
  parseSectionData :: String -> (String, String)
  parseSectionData s = case words s of
    [key, value] -> (key, value)
    ("path":path:_) -> ("path", path)
    ("name":path:_) -> ("name", path)
    _ -> ("", "")

  -- | Helper for 'splitSections'.
  splitSectionsHelper :: ([Section], Maybe Section) -- ^ (accumulator, current section to add to)
                      -> String -- ^ next output line
                      -> ([Section], Maybe Section) -- ^ new accumulator and current section to add to
  splitSectionsHelper (sections, Nothing) s = case parseSectionType s of
      Just sectionType -> (sections, Just (emptySection sectionType))
      Nothing -> (sections, Nothing)
  splitSectionsHelper (sections, Just currentSection) s = case parseSectionType s of
      Just sectionType -> (currentSection:sections, Just (emptySection sectionType))
      Nothing -> (sections, Just (insertSectionData (parseSectionData s) currentSection))

  -- | Takes raw @otool@ output and parses into 'Section's.
  splitSections :: String -> [Section]
  splitSections l = case foldl' splitSectionsHelper ([], Nothing) (lines l) of
                      (reverseSections, Nothing) -> reverse reverseSections
                      (reverseSections, Just lastSection) -> reverse (lastSection:reverseSections)

  -- | Returns 'Just' the path entry if the 'Section' is a @LC_RPATH@ command,
  -- otherwise 'Nothing'.
  rpathOfSection :: Section -> Maybe String
  rpathOfSection (Section LoadCommandSection d) = case (Map.lookup "cmd" d, Map.lookup "path" d) of
    (Just "LC_RPATH", Just path) -> Just path
    _ -> Nothing
  rpathOfSection _ = Nothing

  -- | Takes a list of @otool@ 'Section's and retrieves all @LC_RPATH@ paths from it.
  -- Returns 'Nothing' if there are no 'Section's.
  rpathsFromOtoolSections :: [Section] -> Maybe [String]
  rpathsFromOtoolSections [] = Nothing
  rpathsFromOtoolSections sections = Just (foldr addRpath [] sections)
    where addRpath s acc = case rpathOfSection s of Just rpath -> rpath:acc; _ -> acc

  -- | Takes a file path and returns either 'Nothing',
  --   if it fails to read any binary information (for example if the file is
  --   not readable, or not a binary), or 'Just' the list of successfully read RPATHs.
  readRpaths :: FilePath -> IO (Maybe [String])
  readRpaths filePath = do
    otoolOutput <- otoolLoadCommandOutput filePath
    let rpaths = rpathsFromOtoolSections $ splitSections otoolOutput
    return rpaths
