module PhatSort.Monad.Process
  ( -- * MonadProcess
    MonadProcess(..)
  ) where

-- https://hackage.haskell.org/package/base
import System.IO.Error (tryIOError)

-- https://hackage.haskell.org/package/process
import qualified System.Process as Proc

-- https://hackage.haskell.org/package/transformers
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (ExceptT)

------------------------------------------------------------------------------
-- $MonadProcess

class Monad m => MonadProcess m where
  -- | Run a process
  --
  -- This function runs a process, waits for it to finish, and raises an
  -- exception if it returns a non-zero exit code.
  callProcess
    :: FilePath  -- ^ command
    -> [String]  -- ^ arguments
    -> m (Either IOError ())

instance MonadProcess IO where
  callProcess = (tryIOError .) . Proc.callProcess
  {-# INLINE callProcess #-}

instance MonadProcess m => MonadProcess (ExceptT e m) where
  callProcess = (lift .) . callProcess
  {-# INLINE callProcess #-}
