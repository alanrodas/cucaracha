module Paths_cucaracha (
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

bindir     = "/Users/alanrodas/Proyectos/UNQ/cucaracha/.stack-work/install/x86_64-osx/lts-6.15/7.10.3/bin"
libdir     = "/Users/alanrodas/Proyectos/UNQ/cucaracha/.stack-work/install/x86_64-osx/lts-6.15/7.10.3/lib/x86_64-osx-ghc-7.10.3/cucaracha-0.1.0.0-FplgUcjuVAy8HMM9qW2uWS"
datadir    = "/Users/alanrodas/Proyectos/UNQ/cucaracha/.stack-work/install/x86_64-osx/lts-6.15/7.10.3/share/x86_64-osx-ghc-7.10.3/cucaracha-0.1.0.0"
libexecdir = "/Users/alanrodas/Proyectos/UNQ/cucaracha/.stack-work/install/x86_64-osx/lts-6.15/7.10.3/libexec"
sysconfdir = "/Users/alanrodas/Proyectos/UNQ/cucaracha/.stack-work/install/x86_64-osx/lts-6.15/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cucaracha_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cucaracha_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "cucaracha_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cucaracha_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cucaracha_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
