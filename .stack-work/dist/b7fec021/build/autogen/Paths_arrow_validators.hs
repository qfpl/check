{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_arrow_validators (
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

bindir     = "C:\\Users\\Isaac\\haskell-arrow-validators\\.stack-work\\install\\eb32346d\\bin"
libdir     = "C:\\Users\\Isaac\\haskell-arrow-validators\\.stack-work\\install\\eb32346d\\lib\\x86_64-windows-ghc-8.0.1\\arrow-validators-0.1.0.0-2A3WdFyHsEVJibN4lSG3MR"
datadir    = "C:\\Users\\Isaac\\haskell-arrow-validators\\.stack-work\\install\\eb32346d\\share\\x86_64-windows-ghc-8.0.1\\arrow-validators-0.1.0.0"
libexecdir = "C:\\Users\\Isaac\\haskell-arrow-validators\\.stack-work\\install\\eb32346d\\libexec"
sysconfdir = "C:\\Users\\Isaac\\haskell-arrow-validators\\.stack-work\\install\\eb32346d\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "arrow_validators_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "arrow_validators_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "arrow_validators_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "arrow_validators_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "arrow_validators_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
