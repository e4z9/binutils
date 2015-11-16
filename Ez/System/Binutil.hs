{-# LANGUAGE CPP #-}

-- |
-- Module      :  Ez.System.Binutils
-- Copyright   :  (c) Eike Ziller
-- License     :  MIT
--
-- Stability   :  experimental
-- Portability :  non-portable (OS X - only)
--
-- Tools for investigating binaries.
module Ez.System.Binutil (readRpaths) where

#ifdef darwin_HOST_OS
  import Ez.System.Otool
#elif linux_HOST_OS
  import Ez.System.Elf
#endif
