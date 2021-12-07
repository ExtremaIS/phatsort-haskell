{-# LANGUAGE ForeignFunctionInterface #-}

module PhatSort.Monad.Sync.Unistd
  ( c_sync
  ) where

------------------------------------------------------------------------------

foreign import ccall "unistd.h sync"
  c_sync :: IO ()
