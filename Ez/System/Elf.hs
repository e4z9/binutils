-- |
-- Module      :  Ez.System.Elf
-- Copyright   :  (c) Eike Ziller
-- License     :  MIT
--
-- Stability   :  experimental
-- Portability :  non-portable (ELF - only)
--
-- Running @objdump@ on files and interpreting its output.
module Ez.System.Elf (readRpaths) where

  import Ez.System.Internal

  import System.Process
  import Text.Parsec
  import Text.Parsec.String (Parser)

  -- | Runs @objdump -p@ on the given file path and returns its output.
  objdumpOutput :: FilePath -> IO String
  objdumpOutput filePath = do
    (_, out, _) <- readProcessWithExitCode "objdump" ["-p", filePath] ""
    return out

  pPath :: Parser String
  pPath = many1 (notFollowedBy space >> noneOf [':'])

  pRpathLine :: Parser [String]
  pRpathLine = do
    spaces
    try (string "RPATH") <|> string "RUNPATH"
    spaces
    paths <- pPath `sepBy1` char ':'
    endOfLine
    return paths

  pRpaths :: Parser [String]
  pRpaths = do
    manyTill skipRestOfLine $ try (lookAhead pRpathLine)
    pRpathLine

  parseRpaths :: String -> Maybe [String]
  parseRpaths = eitherToMaybe . runParser pRpaths () ""

  -- | Takes a file path and returns either 'Nothing',
  --   if it fails to read any binary information (for example if the file is
  --   not readable, or not a binary), or 'Just' the list of successfully read RPATHs.
  readRpaths :: FilePath -> IO (Maybe [String])
  readRpaths filePath = fmap parseRpaths (objdumpOutput filePath)
