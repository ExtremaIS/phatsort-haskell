------------------------------------------------------------------------------
-- |
-- Module      : PhatSort.SortOptions
-- Description : sort options
-- Copyright   : Copyright (c) 2021 Travis Cardwell
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
data SortCase
  = CaseSensitive
  | CaseInsensitive
  deriving (Eq, Show)

------------------------------------------------------------------------------

-- | Sorting certain directory entries first
data SortFirst
  = FirstNone
  | FirstDirs
  | FirstFiles
  deriving (Eq, Show)

------------------------------------------------------------------------------

-- | Sort order
data SortOrder
  = OrderName
  | OrderTime
  | OrderRandom
  deriving (Eq, Show)
