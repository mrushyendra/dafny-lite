{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_vcgen (
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

bindir     = "/home/rushy/Desktop/School Stuff/CPSC 454/project1/cs454-haskell/.cabal-sandbox/bin"
libdir     = "/home/rushy/Desktop/School Stuff/CPSC 454/project1/cs454-haskell/.cabal-sandbox/lib/x86_64-linux-ghc-8.2.2/vcgen-0.1.0.0-5bYSN3gTDvpHghzItaeA9Z-vcgen"
dynlibdir  = "/home/rushy/Desktop/School Stuff/CPSC 454/project1/cs454-haskell/.cabal-sandbox/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/rushy/Desktop/School Stuff/CPSC 454/project1/cs454-haskell/.cabal-sandbox/share/x86_64-linux-ghc-8.2.2/vcgen-0.1.0.0"
libexecdir = "/home/rushy/Desktop/School Stuff/CPSC 454/project1/cs454-haskell/.cabal-sandbox/libexec/x86_64-linux-ghc-8.2.2/vcgen-0.1.0.0"
sysconfdir = "/home/rushy/Desktop/School Stuff/CPSC 454/project1/cs454-haskell/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "vcgen_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "vcgen_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "vcgen_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "vcgen_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "vcgen_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "vcgen_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
