module Paths_gstore (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/sebas/.cabal/bin"
libdir     = "/Users/sebas/.cabal/lib/gstore-0.0.0/ghc-6.10.4"
datadir    = "/Users/sebas/.cabal/share/gstore-0.0.0"
libexecdir = "/Users/sebas/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "gstore_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "gstore_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "gstore_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "gstore_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
