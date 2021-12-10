{-# LANGUAGE CPP #-}

module Main (main) where

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (defaultMain, testGroup)

-- (phatsort:test)
#if __GLASGOW_HASKELL__ >= 806
import qualified PhatSort.Cmd.PhatSort.HMock
import qualified PhatSort.Cmd.SeqCp.HMock
#endif

------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "test"
    [
#if __GLASGOW_HASKELL__ >= 806
      PhatSort.Cmd.PhatSort.HMock.tests
    , PhatSort.Cmd.SeqCp.HMock.tests
#endif
    ]
