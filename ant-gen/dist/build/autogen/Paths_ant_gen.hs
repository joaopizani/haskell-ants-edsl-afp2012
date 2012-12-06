module Paths_ant_gen (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/liewe/repos/cs/fp/ants/ant-gen/cabal-dev//bin"
libdir     = "/home/liewe/repos/cs/fp/ants/ant-gen/cabal-dev//lib/ant-gen-0.1.0.0/ghc-7.4.1"
datadir    = "/home/liewe/repos/cs/fp/ants/ant-gen/cabal-dev//share/ant-gen-0.1.0.0"
libexecdir = "/home/liewe/repos/cs/fp/ants/ant-gen/cabal-dev//libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "ant_gen_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ant_gen_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ant_gen_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ant_gen_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
