------------------------------------------------------------------------------
-- |
-- Module      : PhatSort
-- Description : metadata
-- Copyright   : Copyright (c) 2019-2023 Travis Cardwell
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
--
-- @since 0.5.0.0
version :: String
version = "phatsort-haskell " ++ showVersion Project.version
