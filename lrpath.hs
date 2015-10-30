module Main where

  import Control.Monad
  import System.Environment

  import Ez.Concurrent
  import Ez.System.Directory
  import Ez.System.Binutil

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
