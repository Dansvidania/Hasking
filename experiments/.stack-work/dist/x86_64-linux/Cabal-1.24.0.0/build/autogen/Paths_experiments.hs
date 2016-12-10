{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_experiments (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/media/dan/data/code/haskell/hasking/experiments/.stack-work/install/x86_64-linux/lts-7.11/8.0.1/bin"
libdir     = "/media/dan/data/code/haskell/hasking/experiments/.stack-work/install/x86_64-linux/lts-7.11/8.0.1/lib/x86_64-linux-ghc-8.0.1/experiments-0.1.0.0-CnVzBd4qESF6QW57bE4VDa"
datadir    = "/media/dan/data/code/haskell/hasking/experiments/.stack-work/install/x86_64-linux/lts-7.11/8.0.1/share/x86_64-linux-ghc-8.0.1/experiments-0.1.0.0"
libexecdir = "/media/dan/data/code/haskell/hasking/experiments/.stack-work/install/x86_64-linux/lts-7.11/8.0.1/libexec"
sysconfdir = "/media/dan/data/code/haskell/hasking/experiments/.stack-work/install/x86_64-linux/lts-7.11/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "experiments_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "experiments_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "experiments_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "experiments_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "experiments_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
