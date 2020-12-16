{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Logic (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\gupta\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\gupta\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.2\\Logic-0.1.0.0-inplace-logic"
dynlibdir  = "C:\\Users\\gupta\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.2"
datadir    = "C:\\Users\\gupta\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.2\\Logic-0.1.0.0"
libexecdir = "C:\\Users\\gupta\\AppData\\Roaming\\cabal\\Logic-0.1.0.0-inplace-logic\\x86_64-windows-ghc-8.10.2\\Logic-0.1.0.0"
sysconfdir = "C:\\Users\\gupta\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Logic_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Logic_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Logic_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Logic_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Logic_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Logic_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
