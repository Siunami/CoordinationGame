module Paths_CoordinationGame (
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

bindir     = "/home/raghav/Documents/cs312/project2/CoordinationGame/.cabal-sandbox/bin"
libdir     = "/home/raghav/Documents/cs312/project2/CoordinationGame/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/CoordinationGame-0.1.0.0-8sls9FXGiIc0Ljj5Maur9A"
datadir    = "/home/raghav/Documents/cs312/project2/CoordinationGame/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/CoordinationGame-0.1.0.0"
libexecdir = "/home/raghav/Documents/cs312/project2/CoordinationGame/.cabal-sandbox/libexec"
sysconfdir = "/home/raghav/Documents/cs312/project2/CoordinationGame/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "CoordinationGame_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "CoordinationGame_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "CoordinationGame_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "CoordinationGame_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "CoordinationGame_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
