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

-- splitting up into sections
-- possibly do this with Data.List.break instead
  splitSectionsHelper :: [[String]] -> String -> [[String]]
  splitSectionsHelper [] s = [[s]]
  splitSectionsHelper acc s
    | isSectionHeader s = [s]:acc
    where isSectionHeader x = "Section" `isPrefixOf` x || "Load command" `isPrefixOf` x
  splitSectionsHelper (x:acc) s = (s:x):acc

  splitSections :: [String] -> [[String]]
  splitSections l = twoLevelReverse $ foldl' splitSectionsHelper [] l

-- parse rpaths from otool output
  rpathsFromOtool :: String -> [String]
  rpathsFromOtool otoolOutput = intercalate ["--"] $ splitSections $ lines otoolOutput

-- get all rpaths for file path
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
