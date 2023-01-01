------------------------------------------------------------------------------
-- |
-- Module      : PhatSort.Monad.Sync.Unistd
-- Description : sync implementation for POSIX systems
-- Copyright   : Copyright (c) 2019-2023 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module PhatSort.Monad.Sync.Unistd
  ( -- * API
    c_sync
  ) where

------------------------------------------------------------------------------

-- | Call the @sync@ system call
--
-- @since 0.5.0.0
foreign import ccall "unistd.h sync"
  c_sync :: IO ()
