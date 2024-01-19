------------------------------------------------------------------------------
-- |
-- Module      : PhatSort.Monad.FileSystem
-- Description : filesystem I/O
-- Copyright   : Copyright (c) 2019-2024 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

module PhatSort.Monad.FileSystem
  ( -- * MonadFileSystem
    MonadFileSystem(..)
    -- * FileStatus
  , FileStatus(..)
  ) where

-- https://hackage.haskell.org/package/base
import System.IO.Error (tryIOError)
import System.Posix.Types (DeviceID, EpochTime)

-- https://hackage.haskell.org/package/directory
import qualified System.Directory as Dir

-- https://hackage.haskell.org/package/transformers
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (ExceptT)

-- https://hackage.haskell.org/package/unix-compat
import qualified System.PosixCompat.Files as Files

------------------------------------------------------------------------------
-- $MonadFileSystem

-- | Filesystem I/O
--
-- @since 0.5.0.0
class Monad m => MonadFileSystem m where
  -- | Copy a file
  copyFile
    :: FilePath  -- ^ source
    -> FilePath  -- ^ destination
    -> m (Either IOError ())

  -- | Create a directory
  createDirectory :: FilePath -> m (Either IOError ())

  -- | Check if a path exists
  doesPathExist :: FilePath -> m (Either IOError Bool)

  -- | Get file status information
  getFileStatus :: FilePath -> m (Either IOError FileStatus)

  -- | Get a list of directory entries
  listDirectory :: FilePath -> m (Either IOError [FilePath])

  -- | Convert a path into an absolute path
  makeAbsolute :: FilePath -> m (Either IOError FilePath)

  -- | Remove a directory
  removeDirectory :: FilePath -> m (Either IOError ())

  -- | Rename a directory
  renameDirectory
    :: FilePath  -- ^ target directory
    -> FilePath  -- ^ new directory name
    -> m (Either IOError ())

  -- | Rename a file
  renameFile
    :: FilePath  -- ^ target file
    -> FilePath  -- ^ new file name
    -> m (Either IOError ())

instance MonadFileSystem IO where
  copyFile = (tryIOError .) . Dir.copyFile
  {-# INLINE copyFile #-}

  createDirectory = tryIOError . Dir.createDirectory
  {-# INLINE createDirectory #-}

  doesPathExist = tryIOError . Dir.doesPathExist
  {-# INLINE doesPathExist #-}

  getFileStatus = tryIOError . fmap toFileStatus . Files.getFileStatus
  {-# INLINE getFileStatus #-}

  listDirectory = tryIOError . Dir.listDirectory
  {-# INLINE listDirectory #-}

  makeAbsolute = tryIOError . Dir.makeAbsolute
  {-# INLINE makeAbsolute #-}

  removeDirectory = tryIOError . Dir.removeDirectory
  {-# INLINE removeDirectory #-}

  renameDirectory = (tryIOError .) . Dir.renameDirectory
  {-# INLINE renameDirectory #-}

  renameFile = (tryIOError .) . Dir.renameFile
  {-# INLINE renameFile #-}

instance MonadFileSystem m => MonadFileSystem (ExceptT e m) where
  copyFile = (lift .) . copyFile
  {-# INLINE copyFile #-}

  createDirectory = lift . createDirectory
  {-# INLINE createDirectory #-}

  doesPathExist = lift . doesPathExist
  {-# INLINE doesPathExist #-}

  getFileStatus = lift . getFileStatus
  {-# INLINE getFileStatus #-}

  listDirectory = lift . listDirectory
  {-# INLINE listDirectory #-}

  makeAbsolute = lift . makeAbsolute
  {-# INLINE makeAbsolute #-}

  removeDirectory = lift . removeDirectory
  {-# INLINE removeDirectory #-}

  renameDirectory = (lift .) . renameDirectory
  {-# INLINE renameDirectory #-}

  renameFile = (lift .) . renameFile
  {-# INLINE renameFile #-}

------------------------------------------------------------------------------
-- $FileStatus

-- | Mockable subset of 'Files.FileStatus'
--
-- @since 0.5.0.0
data FileStatus
  = FileStatus
    { deviceID         :: !DeviceID
    , isDirectory      :: !Bool
    , modificationTime :: !EpochTime
    }

-- | Convert from 'Files.FileStatus' to 'FileStatus'
--
-- @since 0.5.0.0
toFileStatus :: Files.FileStatus -> FileStatus
toFileStatus status = FileStatus
    { deviceID         = Files.deviceID status
    , isDirectory      = Files.isDirectory status
    , modificationTime = Files.modificationTime status
    }
