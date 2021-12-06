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

class Monad m => MonadStdio m where
  putStrLn :: String -> m ()

instance MonadStdio IO where
  putStrLn = Prelude.putStrLn
  {-# INLINE putStrLn #-}

instance MonadStdio m => MonadStdio (ExceptT e m) where
  putStrLn = lift . putStrLn
  {-# INLINE putStrLn #-}
