module Ez.System.Directory (lazyRecursiveFilePaths) where

  import System.Directory
  import System.FilePath (pathSeparator)
  import System.IO.Unsafe

  lazyRecursiveFilePaths :: FilePath -> IO [FilePath]
  lazyRecursiveFilePaths filePath = unsafeInterleaveIO $ lazyRecursiveFilePathsHelper [filePath]

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
