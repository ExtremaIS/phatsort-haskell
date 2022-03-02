------------------------------------------------------------------------------
-- |
-- Module      : PhatSort.SortOptions
-- Description : sort options
-- Copyright   : Copyright (c) 2019-2022 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

module PhatSort.SortOptions
  ( -- * Sort Options
    SortCase(..)
  , SortFirst(..)
  , SortOrder(..)
  ) where

------------------------------------------------------------------------------

-- | Case sensitivity
--
-- @since 0.5.0.0
data SortCase
  = CaseSensitive
  | CaseInsensitive
  deriving (Eq, Show)

------------------------------------------------------------------------------

-- | Sorting certain directory entries first
--
-- @since 0.5.0.0
data SortFirst
  = FirstNone
  | FirstDirs
  | FirstFiles
  deriving (Eq, Show)

------------------------------------------------------------------------------

-- | Sort order
--
-- @since 0.5.0.0
data SortOrder
  = OrderName
  | OrderTime
  | OrderRandom
  deriving (Eq, Show)
