-- |
-- Module      :  Ez.System.Elf
-- Copyright   :  (c) Eike Ziller
-- License     :  MIT
--
-- Stability   :  experimental
-- Portability :  non-portable (ELF - only)
--
-- Running @chrpath@ on files and interpreting its output.
module Ez.System.Elf (readRpaths) where

  import Data.List.Split
  import System.Process
  import Text.Regex.Posix

  -- | Runs @chrpath -l@ on the given file path and returns its output.
  chrpathListOutput :: FilePath -> IO String
  chrpathListOutput filePath = do
    (_, out, _) <- readProcessWithExitCode "chrpath" ["-l", filePath] ""
    return out

  -- | Takes @chrpath -l@ output and returns a list of RPATHs, or Nothing
  parseRpaths :: String -> Maybe [String]
  parseRpaths [] = Nothing
  parseRpaths output = Just paths
      where pattern = ": R(UN)?PATH=(.*)"
            (_, _, _, groups) = output =~ pattern :: (String, String, String, [String])
            splitPaths [] = []
            splitPaths (h:t) = concatMap (splitOn ":") t
            paths = splitPaths groups

  -- | Takes a file path and returns either 'Nothing',
  --   if it fails to read any binary information (for example if the file is
  --   not readable, or not a binary), or 'Just' the list of successfully read RPATHs.
  readRpaths :: FilePath -> IO (Maybe [String])
  readRpaths filePath = fmap parseRpaths (chrpathListOutput filePath)
