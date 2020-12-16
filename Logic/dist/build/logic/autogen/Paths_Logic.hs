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

bindir     = "/home/evan/.cabal/bin"
libdir     = "/home/evan/.cabal/lib/x86_64-linux-ghc-8.6.5/Logic-0.1.0.0-KVUbkVxEgMU7WtdARFjghH-logic"
dynlibdir  = "/home/evan/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/evan/.cabal/share/x86_64-linux-ghc-8.6.5/Logic-0.1.0.0"
libexecdir = "/home/evan/.cabal/libexec/x86_64-linux-ghc-8.6.5/Logic-0.1.0.0"
sysconfdir = "/home/evan/.cabal/etc"

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
  return (dir ++ "/" ++ name)
