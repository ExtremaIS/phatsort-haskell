{-# LANGUAGE CPP #-}

module Main (main) where

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (defaultMain, testGroup)

-- (phatsort:test)
#if __GLASGOW_HASKELL__ >= 806
import qualified PhatSort.Cmd.PhatSort.Mock
import qualified PhatSort.Cmd.SeqCp.Mock
#endif

------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "test"
    [
#if __GLASGOW_HASKELL__ >= 806
      PhatSort.Cmd.PhatSort.Mock.tests
    , PhatSort.Cmd.SeqCp.Mock.tests
#endif
    ]
