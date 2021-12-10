------------------------------------------------------------------------------
-- |
-- Module      : PhatSort.Monad.Sync.Unistd
-- Description : sync implementation for POSIX systems
-- Copyright   : Copyright (c) 2019-2021 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module PhatSort.Monad.Sync.Unistd
  ( -- * API
    c_sync
  ) where

------------------------------------------------------------------------------

-- | Call the @sync@ system call
foreign import ccall "unistd.h sync"
  c_sync :: IO ()
