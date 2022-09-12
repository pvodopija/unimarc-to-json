{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_unimarc_haskell (
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

bindir     = "/Users/pavlevodopija/code/unimarc-to-json/.stack-work/install/x86_64-osx/90daf12d3b8dae1a17b048124dbb4aebcf237edb6c1d1882b867c7feccc32d9a/8.10.7/bin"
libdir     = "/Users/pavlevodopija/code/unimarc-to-json/.stack-work/install/x86_64-osx/90daf12d3b8dae1a17b048124dbb4aebcf237edb6c1d1882b867c7feccc32d9a/8.10.7/lib/x86_64-osx-ghc-8.10.7/unimarc-haskell-0.1.0.0-GkKmlV99rrb8z4Wy0mdl4y-unimarc-haskell"
dynlibdir  = "/Users/pavlevodopija/code/unimarc-to-json/.stack-work/install/x86_64-osx/90daf12d3b8dae1a17b048124dbb4aebcf237edb6c1d1882b867c7feccc32d9a/8.10.7/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/pavlevodopija/code/unimarc-to-json/.stack-work/install/x86_64-osx/90daf12d3b8dae1a17b048124dbb4aebcf237edb6c1d1882b867c7feccc32d9a/8.10.7/share/x86_64-osx-ghc-8.10.7/unimarc-haskell-0.1.0.0"
libexecdir = "/Users/pavlevodopija/code/unimarc-to-json/.stack-work/install/x86_64-osx/90daf12d3b8dae1a17b048124dbb4aebcf237edb6c1d1882b867c7feccc32d9a/8.10.7/libexec/x86_64-osx-ghc-8.10.7/unimarc-haskell-0.1.0.0"
sysconfdir = "/Users/pavlevodopija/code/unimarc-to-json/.stack-work/install/x86_64-osx/90daf12d3b8dae1a17b048124dbb4aebcf237edb6c1d1882b867c7feccc32d9a/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "unimarc_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "unimarc_haskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "unimarc_haskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "unimarc_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "unimarc_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "unimarc_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
