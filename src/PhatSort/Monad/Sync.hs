------------------------------------------------------------------------------
-- |
-- Module      : PhatSort.Monad.Sync
-- Description : commit all data in filesystem buffers
-- Copyright   : Copyright (c) 2019-2023 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module PhatSort.Monad.Sync
  ( -- * MonadSync
    MonadSync(..)
  ) where

-- https://hackage.haskell.org/package/transformers
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (ExceptT)

-- (phatsort)
#ifndef mingw32_HOST_OS
import PhatSort.Monad.Sync.Unistd (c_sync)
#endif

------------------------------------------------------------------------------
-- $MonadSync

-- | Commit all data in filesystem buffers
--
-- @since 0.5.0.0
class Monad m => MonadSync m where
  -- | Commit all data in filesystem buffers
  --
  -- This method calls the @sync@ system call on POSIX systems.  It does
  -- nothing on Windows.
  sync :: m ()

instance MonadSync IO where
#ifdef mingw32_HOST_OS
  sync = pure ()
#else
  sync = c_sync
#endif

instance MonadSync m => MonadSync (ExceptT e m) where
  sync = lift sync
  {-# INLINE sync #-}
