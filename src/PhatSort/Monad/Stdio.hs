------------------------------------------------------------------------------
-- |
-- Module      : PhatSort.Monad.Stdio
-- Description : standard I/O
-- Copyright   : Copyright (c) 2019-2024 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

module PhatSort.Monad.Stdio
  ( -- * MonadStdio
    MonadStdio(..)
  ) where

-- https://hackage.haskell.org/package/base
import qualified Prelude
import Prelude hiding (putStrLn)

-- https://hackage.haskell.org/package/transformers
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (ExceptT)

------------------------------------------------------------------------------
-- $MonadStdio

-- | Standard I/O
--
-- @since 0.5.0.0
class Monad m => MonadStdio m where
  -- | Write a 'String' to @STDOUT@, appending a newline
  putStrLn :: String -> m ()

instance MonadStdio IO where
  putStrLn = Prelude.putStrLn
  {-# INLINE putStrLn #-}

instance MonadStdio m => MonadStdio (ExceptT e m) where
  putStrLn = lift . putStrLn
  {-# INLINE putStrLn #-}
