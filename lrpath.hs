module Main where

  import Data.List
  import System.Environment
  import System.Process

  twoLevelReverse :: [[a]] -> [[a]]
  twoLevelReverse = foldl (\ acc l -> reverse l : acc) []

  otoolLoadCommandOutput :: FilePath -> IO String
  otoolLoadCommandOutput filePath = do
    (_, out, _) <- readProcessWithExitCode "otool" ["-l", filePath] ""
    return out

  rpathsFromOtool :: String -> [String]
  rpathsFromOtool otoolOutput = lines otoolOutput

  readRpaths :: FilePath -> IO [String]
  readRpaths filePath = do
    otoolOutput <- otoolLoadCommandOutput filePath
    let rpaths = rpathsFromOtool otoolOutput
    return rpaths

  main :: IO ()
  main = do
    args <- getArgs
    let filePath = head args
    rpaths <- readRpaths filePath
    putStr $ intercalate "\n" rpaths
