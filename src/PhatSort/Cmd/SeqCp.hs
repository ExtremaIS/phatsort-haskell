------------------------------------------------------------------------------
-- |
-- Module      : PhatSort.Cmd.SeqCp
-- Description : seqcp command implementation
-- Copyright   : Copyright (c) 2019-2023 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PhatSort.Cmd.SeqCp
  ( -- * Options
    Options(..)
    -- * API
  , runIO
  , run
  ) where

-- https://hackage.haskell.org/package/base
import Control.Monad (forM, forM_, unless, when)
import Data.Char (toLower)
import Data.List (dropWhileEnd, isPrefixOf, partition, sortBy)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import Data.Ord (comparing)

-- https://hackage.haskell.org/package/filepath
import System.FilePath ((</>), splitDirectories, takeFileName)

-- https://hackage.haskell.org/package/MonadRandom
import Control.Monad.Random.Class (MonadRandom)

-- https://hackage.haskell.org/package/random-shuffle
import System.Random.Shuffle (shuffleM)

-- (phatsort)
import qualified PhatSort.Monad.FileSystem as FS
import PhatSort.Monad.FileSystem (MonadFileSystem)
import qualified PhatSort.Monad.Stdio as Stdio
import PhatSort.Monad.Stdio (MonadStdio)
import qualified PhatSort.Monad.Sync as Sync
import PhatSort.Monad.Sync (MonadSync)
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
    { optCase        :: !SortCase
    , optFirst       :: !SortFirst
    , optSync        :: !Bool
    , optOrder       :: !SortOrder
    , optReverse     :: !Bool
    , optScript      :: !Bool
    , optVerbose     :: !Bool
    , optSources     :: !(NonEmpty FilePath)
    , optDestination :: !FilePath
    }
  deriving Show

------------------------------------------------------------------------------
-- $API

-- | Run the command in the 'IO' monad
runIO :: Options -> IO (Either String ())
runIO = Error.run . run

------------------------------------------------------------------------------

-- | Run the command
run
  :: forall m
   . (MonadFileSystem m, MonadRandom m, MonadStdio m, MonadSync m)
  => Options
  -> ErrorT m ()
run Options{..} = copyTargets =<< getArgTargets optSources optDestination
  where
    copyDir :: FilePath -> FilePath -> FilePath -> ErrorT m ()
    copyDir argPath srcPath dstPath = do
      putProgress argPath
      mkDir dstPath >> sync
      copyTargets =<< getDirTargets argPath srcPath dstPath

    copyFile :: FilePath -> FilePath -> FilePath -> ErrorT m ()
    copyFile argPath srcPath dstPath = do
      putProgress argPath
      cp srcPath dstPath >> sync

    copyTargets :: [Target] -> ErrorT m ()
    copyTargets targets = do
      sortedTargets <- sortTargets targets
      forM_ sortedTargets $ \Target{..} ->
        if FS.isDirectory targetStatus
          then copyDir targetArgPath targetSrcPath targetDstPath
          else copyFile targetArgPath targetSrcPath targetDstPath

    cp :: FilePath -> FilePath -> ErrorT m ()
    cp srcPath dstPath
      | optScript =
          Stdio.putStrLn $ Script.formatCommand ["cp", srcPath, dstPath]
      | otherwise = Error.errorTE $ FS.copyFile srcPath dstPath

    mkDir :: FilePath -> ErrorT m ()
    mkDir path
      | optScript = Stdio.putStrLn $ Script.formatCommand ["mkdir", path]
      | otherwise = Error.errorTE $ FS.createDirectory path

    putProgress :: FilePath -> ErrorT m ()
    putProgress path
      | not optVerbose = pure ()
      | optScript = Stdio.putStrLn $ Script.formatCommand ["echo", path]
      | otherwise = Stdio.putStrLn path

    sortTargets :: [Target] -> ErrorT m [Target]
    sortTargets targets =
      let compareNames = case optCase of
            CaseSensitive -> comparing targetName
            CaseInsensitive -> comparing (map toLower . targetName)
          compareTimes = comparing (FS.modificationTime . targetStatus)
          reverse' = if optReverse then reverse else id
          go = case optOrder of
            OrderName -> pure . reverse' . sortBy compareNames
            OrderTime -> pure . reverse' . sortBy compareTimes
            OrderRandom -> shuffleM
          (dirTargets, fileTargets) =
            partition (FS.isDirectory . targetStatus) targets
      in  Error.lift $ case optFirst of
            FirstNone -> go targets
            FirstDirs -> (++) <$> go dirTargets <*> go fileTargets
            FirstFiles -> (++) <$> go fileTargets <*> go dirTargets

    sync :: ErrorT m ()
    sync
      | not optSync = pure ()
      | optScript = Stdio.putStrLn $ Script.formatCommand ["sync"]
      | otherwise = Sync.sync

------------------------------------------------------------------------------
-- $Internal

-- | Target file or directory
data Target
  = Target
    { targetArgPath :: !FilePath  -- ^ path for verbose and errors
    , targetSrcPath :: !FilePath  -- ^ absolute source path
    , targetDstPath :: !FilePath  -- ^ absolute destination path
    , targetName    :: !FilePath  -- ^ file or directory name
    , targetStatus  :: !FS.FileStatus
    }

------------------------------------------------------------------------------

-- | Create 'Target's for arguments, performing checks
getArgTargets
  :: MonadFileSystem m
  => NonEmpty FilePath  -- ^ sources
  -> FilePath           -- ^ destination directory
  -> ErrorT m [Target]
getArgTargets sources destDir = do
    destPath <- Error.errorTE .
      FS.makeAbsolute $ dropWhileEnd (== '/') destDir
    destStatus <- Error.errorTE $ FS.getFileStatus destPath
    unless (FS.isDirectory destStatus) .
      Error.throw $ "not a directory: " ++ destDir
    let destDirs = splitDirectories destPath
    forM (NonEmpty.toList sources) $ \targetArgPath -> do
      targetSrcPath <- Error.errorTE .
        FS.makeAbsolute $ dropWhileEnd (== '/') targetArgPath
      let targetName = takeFileName targetSrcPath
          targetDstPath = destPath </> targetName
      targetStatus <- Error.errorTE $ FS.getFileStatus targetSrcPath
      when (FS.isDirectory targetStatus) .
        when (splitDirectories targetSrcPath `isPrefixOf` destDirs) $
          Error.throw $
            "source directory above target directory: " ++ targetArgPath
      exists <- Error.errorTE $ FS.doesPathExist targetDstPath
      when exists . Error.throw $ "already exists: " ++ targetDstPath
      pure Target{..}

------------------------------------------------------------------------------

-- | Get 'Target's for entries in a directory
getDirTargets
  :: MonadFileSystem m
  => FilePath  -- ^ path for verbose and errors
  -> FilePath  -- ^ absolute source path
  -> FilePath  -- ^ absolute destination path
  -> ErrorT m [Target]
getDirTargets argDir srcDir dstDir = do
    names <- Error.errorTE $ FS.listDirectory srcDir
    forM names $ \targetName -> do
      let targetArgPath = argDir </> targetName
          targetSrcPath = srcDir </> targetName
          targetDstPath = dstDir </> targetName
      targetStatus <- Error.errorTE $ FS.getFileStatus targetSrcPath
      pure Target{..}
