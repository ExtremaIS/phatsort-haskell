------------------------------------------------------------------------------
-- |
-- Module      : PhatSort
-- Description : metadata
-- Copyright   : Copyright (c) 2019-2021 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

module PhatSort
  ( -- * Constants
    version
  ) where

-- https://hackage.haskell.org/package/base
import Data.Version (showVersion)

-- (phatsort:cabal)
import qualified Paths_phatsort as Project

------------------------------------------------------------------------------
-- $Constants

-- | PhatSort version string (\"@phatsort-haskell X.X.X.X@\")
version :: String
version = "phatsort-haskell " ++ showVersion Project.version
