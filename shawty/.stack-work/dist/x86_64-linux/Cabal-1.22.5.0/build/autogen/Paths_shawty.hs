module Paths_shawty (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/media/dan/data/code/haskell/hasking/shawty/.stack-work/install/x86_64-linux/lts-6.26/7.10.3/bin"
libdir     = "/media/dan/data/code/haskell/hasking/shawty/.stack-work/install/x86_64-linux/lts-6.26/7.10.3/lib/x86_64-linux-ghc-7.10.3/shawty-0.1.0.0-AwOVwLE4y4a3vnpl7IwVzP"
datadir    = "/media/dan/data/code/haskell/hasking/shawty/.stack-work/install/x86_64-linux/lts-6.26/7.10.3/share/x86_64-linux-ghc-7.10.3/shawty-0.1.0.0"
libexecdir = "/media/dan/data/code/haskell/hasking/shawty/.stack-work/install/x86_64-linux/lts-6.26/7.10.3/libexec"
sysconfdir = "/media/dan/data/code/haskell/hasking/shawty/.stack-work/install/x86_64-linux/lts-6.26/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "shawty_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "shawty_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "shawty_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "shawty_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "shawty_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
