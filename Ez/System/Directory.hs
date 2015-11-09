-- |
-- Module      :  Ez.System.Directory
-- Copyright   :  (c) Eike Ziller
-- License     :  MIT
--
-- Stability   :  experimental
-- Portability :  portable
--
-- Directory related utility functions.

module Ez.System.Directory (lazyRecursiveFilePaths) where

  import System.Directory
  import System.FilePath (pathSeparator)
  import System.IO.Unsafe

  -- | Creates a lazy list of all files in the file system hierarchy starting
  --   with the given file path.
  --   Returns just the file path if that is a file instead of a directory.
  --   Returns an empty list if the given file path does not exist.
  lazyRecursiveFilePaths :: FilePath -> IO [FilePath]
  lazyRecursiveFilePaths filePath = unsafeInterleaveIO $ lazyRecursiveFilePathsHelper [filePath]

  -- | Helper function for 'lazyRecursiveFilePaths'. Takes the list of pending
  --   file paths to process.
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
