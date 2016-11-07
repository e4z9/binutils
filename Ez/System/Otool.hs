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

  import Ez.System.Internal

  import Control.Monad (mfilter, void)
  import Data.List
  import Data.Maybe (mapMaybe)
  import Data.Map.Strict (Map)
  import qualified Data.Map.Strict as Map
  import System.Process
  import Text.Parsec
  import Text.Parsec.String (Parser)

  -- | Runs @otool -l@ on the given file path and returns its output.
  otoolLoadCommandOutput :: FilePath -> IO String
  otoolLoadCommandOutput filePath = do
    (_, out, _) <- readProcessWithExitCode "otool" ["-l", filePath] ""
    return out

  -- | Type of a Mach-O load command section.
  data SectionHeader = LoadCommandSection Integer -- ^ Load command.
      | OtherSection -- ^ Other section.
      deriving (Show)

  -- | One Mach-O load command section with key-value pairs.
  data Section = Section SectionHeader (Map String String) -- ^ Creates a section with given type and data.

  -- | Shows 'Section' type and key-value map.
  instance Show Section where
    show (Section t d) = "Section: " ++ show t ++ " " ++ show d

  pLoadCommandH :: Parser SectionHeader
  pLoadCommandH = do
    string "Load command"
    spaces
    n <- pNumber
    endOfLine
    return $ LoadCommandSection n

  pSectionH :: Parser SectionHeader
  pSectionH = do
    string "Section"
    manyTill space $ try endOfLine
    return OtherSection

  pHeader :: Parser SectionHeader
  pHeader = try pLoadCommandH <|> pSectionH

  pOffset :: Parser Integer
  pOffset = do
    spaces
    string "(offset"
    spaces
    n <- pNumber
    char ')'
    return n

  skipMachHeader :: Parser ()
  skipMachHeader = do
    string "Mach header"
    endOfLine
    skipRestOfLine -- titles
    skipRestOfLine -- data

  pEntryValue :: Parser String
  pEntryValue = do
    value <- manyTill anyChar
      $ try (void pOffset) <|> try (lookAhead (void endOfLine))
    endOfLine
    return value

  pEntryWithName :: Parser String -> Parser (String, String)
  pEntryWithName name = do
    spaces
    key <- name
    spaces
    value <- pEntryValue
    return (key, value)

  pEntry :: Parser (String, String)
  pEntry = try (pEntryWithName $ string "time stamp")
    <|> try (pEntryWithName $ string "current version")
    <|> try (pEntryWithName $ string "compatibility version")
    <|> pEntryWithName word

  pSection :: Parser Section
  pSection = do
    header <- pHeader
    entries <- manyTill pEntry headerOrEnd
    return $ Section header (Map.fromList entries)
    where headerOrEnd = void (try $ lookAhead pHeader) <|> eof

  pSections :: Parser [Section]
  pSections = do
    pFileName
    skipMachHeader
    sections <- many1 pSection
    spaces
    eof
    return sections
    where pFileName = skipRestOfLine

  parseSections :: String -> Maybe [Section]
  parseSections = eitherToMaybe . runParser pSections () ""

  -- | Returns 'Just' the path entry if the 'Section' is a @LC_RPATH@ command,
  -- otherwise 'Nothing'.
  rpathOfSection :: Section -> Maybe String
  rpathOfSection (Section (LoadCommandSection _) values) = do
    mfilter (== "LC_RPATH") $ Map.lookup "cmd" values
    Map.lookup "path" values
  rpathOfSection _ = Nothing

  -- | Takes a list of @otool@ 'Section's and retrieves all @LC_RPATH@ paths from it.
  -- Returns 'Nothing' if there are no 'Section's.
  rpathsFromOtoolSections :: Maybe [Section] -> Maybe [String]
  rpathsFromOtoolSections maybeSections = do
    sections <- maybeSections
    return $ mapMaybe rpathOfSection sections

  -- | Takes a file path and returns either 'Nothing',
  --   if it fails to read any binary information (for example if the file is
  --   not readable, or not a binary), or 'Just' the list of successfully read RPATHs.
  readRpaths :: FilePath -> IO (Maybe [String])
  readRpaths filePath = do
    otoolOutput <- otoolLoadCommandOutput filePath
    return $ rpathsFromOtoolSections $ parseSections otoolOutput
