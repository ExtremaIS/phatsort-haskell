{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PhatSort.Cmd.PhatSort
  ( -- * Options
    Options(..)
    -- * API
  , runIO
  , run
  ) where

-- https://hackage.haskell.org/package/base
import Control.Monad (forM, forM_, unless, when)
import Data.Char (toLower)
import Data.List (dropWhileEnd, isSuffixOf, partition, sortBy)
import Data.Ord (comparing)

-- https://hackage.haskell.org/package/filepath
import System.FilePath ((</>), takeDirectory)

-- https://hackage.haskell.org/package/MonadRandom
import Control.Monad.Random.Class (MonadRandom)

-- -- https://hackage.haskell.org/package/random-shuffle
import System.Random.Shuffle (shuffleM)

-- (phatsort)
import qualified PhatSort.Monad.FileSystem as FS
import PhatSort.Monad.FileSystem (MonadFileSystem)
import qualified PhatSort.Monad.Process as Proc
import PhatSort.Monad.Process (MonadProcess)
import qualified PhatSort.Monad.Stdio as Stdio
import PhatSort.Monad.Stdio (MonadStdio)
import qualified PhatSort.Monad.Trans.Error as Error
import PhatSort.Monad.Trans.Error (ErrorT)
import qualified PhatSort.Script as Script
import PhatSort.SortOptions
  ( SortCase(CaseInsensitive, CaseSensitive)
  , SortFirst(FirstDirs, FirstFiles, FirstNone)
  , SortOrder(OrderName, OrderRandom, OrderTime)
  )

------------------------------------------------------------------------------
-- $Options

data Options
  = Options
    { optCase    :: !SortCase
    , optFirst   :: !SortFirst
    , optSync    :: !Bool
    , optOrder   :: !SortOrder
    , optReverse :: !Bool
    , optScript  :: !Bool
    , optVerbose :: !Bool
    , optTargets :: ![FilePath]
    }
  deriving Show

------------------------------------------------------------------------------
-- $API

runIO :: Options -> IO (Either String ())
runIO = Error.run . run

------------------------------------------------------------------------------

run
  :: forall m
   . (MonadFileSystem m, MonadProcess m, MonadRandom m, MonadStdio m)
  => Options
  -> ErrorT m ()
run Options{..} = do
    targets <- mapM getTarget optTargets
    forM_ targets $ \Target{..} -> do
      putProgress targetArgPath
      mvDir targetDstPath targetSrcPath >> sync
      mkDir targetDstPath >> sync
      processDir targetArgPath targetSrcPath targetDstPath
      rmDir targetSrcPath >> sync
  where
    mkDir :: FilePath -> ErrorT m ()
    mkDir path
      | optScript = Stdio.putStrLn $ Script.formatCommand ["mkdir", path]
      | otherwise = Error.errorTE $ FS.createDirectory path

    mvDir :: FilePath -> FilePath -> ErrorT m ()
    mvDir srcPath dstPath
      | optScript =
          Stdio.putStrLn $ Script.formatCommand ["mv", srcPath, dstPath]
      | otherwise = Error.errorTE $ FS.renameDirectory srcPath dstPath

    mvFile :: FilePath -> FilePath -> ErrorT m ()
    mvFile srcPath dstPath
      | optScript =
          Stdio.putStrLn $ Script.formatCommand ["mv", srcPath, dstPath]
      | otherwise = Error.errorTE $ FS.renameFile srcPath dstPath

    processDir :: FilePath -> FilePath -> FilePath -> ErrorT m ()
    processDir argPath srcPath dstPath = do
      let goDir Entry{..} = do
            putProgress entryArgPath
            mkDir entryDstPath >> sync
            processDir entryArgPath entrySrcPath entryDstPath
            rmDir entrySrcPath >> sync
          goFile Entry{..} = do
            putProgress entryArgPath
            mvFile entrySrcPath entryDstPath >> sync
          go entry@Entry{..}
            | FS.isDirectory entryStatus = goDir entry
            | otherwise = goFile entry
      allEntries <- getEntries optScript argPath srcPath dstPath
      let (dirEntries, fileEntries) =
            partition (FS.isDirectory . entryStatus) allEntries
      case optFirst of
        FirstNone -> mapM_ go =<< sortEntries allEntries
        FirstDirs -> do
          mapM_ goDir =<< sortEntries dirEntries
          mapM_ goFile =<< sortEntries fileEntries
        FirstFiles -> do
          mapM_ goFile =<< sortEntries fileEntries
          mapM_ goDir =<< sortEntries dirEntries

    putProgress :: FilePath -> ErrorT m ()
    putProgress path
      | not optVerbose = pure ()
      | optScript = Stdio.putStrLn $ Script.formatCommand ["echo", path]
      | otherwise = Stdio.putStrLn path

    rmDir :: FilePath -> ErrorT m ()
    rmDir path
      | optScript = Stdio.putStrLn $ Script.formatCommand ["rmdir", path]
      | otherwise = Error.errorTE $ FS.removeDirectory path

    sortEntries :: [Entry] -> ErrorT m [Entry]
    sortEntries entries = Error.lift $ do
      let compareNames = case optCase of
            CaseSensitive -> comparing entryName
            CaseInsensitive -> comparing (map toLower . entryName)
          compareTimes = comparing (FS.modificationTime . entryStatus)
          reverse' = if optReverse then reverse else id
      case optOrder of
        OrderName -> pure . reverse' $ sortBy compareNames entries
        OrderTime -> pure . reverse' $ sortBy compareTimes entries
        OrderRandom -> shuffleM entries

    sync :: ErrorT m ()
    sync
      | not optSync = pure ()
      | optScript = Stdio.putStrLn $ Script.formatCommand ["sync"]
      | otherwise = Error.errorTE $ Proc.callProcess "sync" []

------------------------------------------------------------------------------
-- $Internal

-- | Target directory
data Target
  = Target
    { targetArgPath :: !FilePath
    , targetSrcPath :: !FilePath
    , targetDstPath :: !FilePath
    }

------------------------------------------------------------------------------

getTarget
  :: MonadFileSystem m
  => FilePath
  -> ErrorT m Target
getTarget targetArgPath = do
    when ("-phat" `isSuffixOf` targetArgPath) .
      Error.throw $ "-phat directory: " ++ targetArgPath
    targetDstPath <- Error.errorTE .
      FS.makeAbsolute $ dropWhileEnd (== '/') targetArgPath
    tgtStatus <- Error.errorTE $ FS.getFileStatus targetDstPath
    unless (FS.isDirectory tgtStatus) .
      Error.throw $ "not a directory: " ++ targetArgPath
    let parentDir = takeDirectory targetDstPath
    parentStatus <- Error.errorTE $ FS.getFileStatus parentDir
    unless (FS.deviceID tgtStatus == FS.deviceID parentStatus) .
      Error.throw $ "mount point: " ++ targetArgPath
    let targetSrcPath = targetDstPath ++ "-phat"
    exists <- Error.errorTE $ FS.doesPathExist targetSrcPath
    when exists . Error.throw $ "already exists: " ++ targetArgPath ++ "-phat"
    pure Target{..}

------------------------------------------------------------------------------

-- | Directory entry
data Entry
  = Entry
    { entryName    :: !FilePath
    , entryArgPath :: !FilePath
    , entrySrcPath :: !FilePath
    , entryDstPath :: !FilePath
    , entryStatus  :: !FS.FileStatus
    }

------------------------------------------------------------------------------

-- | Get entries in the specified directory
getEntries
  :: MonadFileSystem m
  => Bool      -- ^ script?
  -> FilePath  -- ^ arg directory
  -> FilePath  -- ^ source directory
  -> FilePath  -- ^ destination directory
  -> ErrorT m [Entry]
getEntries isScript argDir srcDir dstDir = do
    names <- Error.errorTE $
      FS.listDirectory (if isScript then dstDir else srcDir)
    forM names $ \entryName -> do
      let entryArgPath = argDir </> entryName
          entrySrcPath = srcDir </> entryName
          entryDstPath = dstDir </> entryName
      entryStatus <- Error.errorTE $
        FS.getFileStatus (if isScript then entryDstPath else entrySrcPath)
      pure Entry{..}
