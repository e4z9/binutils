{-# LANGUAGE CPP #-}

module Ez.System.Binutil (readRpaths) where

#ifdef darwin_HOST_OS
  import Ez.System.Otool
#endif
