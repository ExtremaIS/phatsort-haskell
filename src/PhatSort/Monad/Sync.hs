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

-- commit filesystem caches to disk
class Monad m => MonadSync m where
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
