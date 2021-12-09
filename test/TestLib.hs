{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestLib where

-- https://hackage.haskell.org/package/base
import Control.Monad.IO.Class (MonadIO(liftIO))
import GHC.Stack (HasCallStack)

-- https://hackage.haskell.org/package/HMock
import Test.HMock (MockT, makeMockable)

-- https://hackage.haskell.org/package/MonadRandom
import qualified Control.Monad.Random.Lazy as Rand
import Control.Monad.Random.Lazy (MonadRandom)

-- https://hackage.haskell.org/package/tasty-hunit
import qualified Test.Tasty.HUnit as HUnit

-- https://hackage.haskell.org/package/transformers
import Control.Monad.Trans.Class (lift)

-- (phatsort)
import PhatSort.Monad.FileSystem (MonadFileSystem)
import PhatSort.Monad.Stdio (MonadStdio)
import PhatSort.Monad.Sync (MonadSync)

------------------------------------------------------------------------------

makeMockable [t|MonadFileSystem|]

makeMockable [t|MonadStdio|]

makeMockable [t|MonadSync|]

------------------------------------------------------------------------------

instance MonadRandom m => MonadRandom (MockT m) where
  getRandomR  = lift . Rand.getRandomR
  getRandom   = lift   Rand.getRandom
  getRandomRs = lift . Rand.getRandomRs
  getRandoms  = lift   Rand.getRandoms

------------------------------------------------------------------------------

assertSuccess
  :: (HasCallStack, MonadIO m)
  => Either String ()
  -> m ()
assertSuccess = \case
    Right () -> pure ()
    Left err -> liftIO . HUnit.assertFailure $ "unexpected error: " ++ err

------------------------------------------------------------------------------

assertError
  :: (HasCallStack, MonadIO m)
  => String  -- ^ expected error message
  -> Either String ()
  -> m ()
assertError expected = \case
    Left err
      | err == expected -> pure ()
      | otherwise -> liftIO . HUnit.assertFailure $ unlines
          [ "unexpected error message"
          , "  expected: " ++ expected
          , "  but got:  " ++ err
          ]
    Right () -> liftIO . HUnit.assertFailure $ unlines
      [ "unexpected success"
      , "  expected error message: " ++ expected
      ]
