module Finder
    ( findFile
    , findDirectory
    , findWithFilter
    ) where

import           Control.Monad       (filterM)
import           Data.List           (inits)
import           Data.Maybe          (listToMaybe)
import qualified System.Directory    as SD
import           System.Environment  (getExecutablePath)
import           System.FilePath     (dropFileName, joinPath, splitDirectories)

findFile :: (FilePath -> FilePath) -> IO (Maybe FilePath)
findFile path = findWithFilter path SD.doesFileExist

findDirectory :: (FilePath -> FilePath) -> IO (Maybe FilePath)
findDirectory path = findWithFilter path SD.doesDirectoryExist

findWithFilter :: (FilePath -> FilePath) -> (FilePath -> IO Bool) -> IO (Maybe FilePath)
findWithFilter findPath findFilter = do
    exeDir <- getExecutableDirectory
    let potentialLocations = fmap findPath (getParentDirectories exeDir)
    listToMaybe <$> filterM findFilter potentialLocations

getExecutableDirectory :: IO FilePath
getExecutableDirectory = fmap dropFileName getExecutablePath

getParentDirectories :: FilePath -> [FilePath]
getParentDirectories = reverse . fmap joinPath . tail . inits . splitDirectories

