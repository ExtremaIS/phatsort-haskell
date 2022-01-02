------------------------------------------------------------------------------
-- |
-- Module      : PhatSort.Script
-- Description : script API
-- Copyright   : Copyright (c) 2019-2022 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

module PhatSort.Script
  ( -- * API
    formatCommand
  ) where

-- https://hackage.haskell.org/package/base
import Data.Char (ord)

------------------------------------------------------------------------------

-- | Format a shell command, escaping when necessary
formatCommand
  :: [String]  -- ^ command and arguments
  -> String
formatCommand = unwords . map escape
  where
    escape :: String -> String
    escape s
      | all isOkChar s = s
      | otherwise = "'" ++ concatMap tr s ++ "'"

    isOkChar :: Char -> Bool
    isOkChar c =
      let c' = ord c
      in  or
            [ (c' - ord 'a') <= 25
            , (c' - ord 'A') <= 25
            , (c' - ord '0') <= 9
            , c `elem` (".-+_/" :: String)
            ]

    tr :: Char -> String
    tr '\'' = "'\"'\"'"
    tr '\0' = ""
    tr c    = [c]
