-- |
-- Module      :  Main
-- Copyright   :  (c) Eike Ziller
-- License     :  MIT
--
-- Stability   :  experimental
-- Portability :  non-portable (OS X - only)
--
-- Print all RPATHs found in all binaries in a directory hierarchy.
module Main where

  import Control.Monad
  import System.Environment

  import Ez.Concurrent
  import Ez.System.Directory
  import Ez.System.Binutil

  -- | For a given file path returns a tuple with that file path and the
  --   file's RPATHs (if any).
  mapRpaths :: FilePath -> IO (FilePath, Maybe [String])
  mapRpaths filePath = do
      rpaths <- readRpaths filePath
      return (filePath, rpaths)

  -- | Prints the file path in one line, and the RPATHs indented each in a
  --   separate line afterwards (if any).
  --   Prints the file path followed by __None__ if the RPATH list is empty.
  --   Does not print anything if the RPATH list is 'Nothing'.
  printRpaths :: (FilePath, Maybe [String]) -> IO ()
  printRpaths (_, Nothing) = return ()
  printRpaths (filePath, Just rpaths) = do
      putStr filePath >> putStr ":"
      when (null rpaths) $ putStr " None"
      putStrLn ""
      putPrefixed "    " rpaths
      where putPrefixed indent = mapM_ $ putStrLn . (indent ++)

  -- | main
  main :: IO ()
  main = do
    args <- getArgs
    filePaths <- fmap concat (mapM lazyRecursiveFilePaths args)
    mapReduceIO mapRpaths 8 (const printRpaths) () filePaths
