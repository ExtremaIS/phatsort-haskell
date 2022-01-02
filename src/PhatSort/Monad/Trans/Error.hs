------------------------------------------------------------------------------
-- |
-- Module      : PhatSort.Monad.Trans.Error
-- Description : error monad transformer
-- Copyright   : Copyright (c) 2019-2022 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

module PhatSort.Monad.Trans.Error
  ( -- * ErrorT
    ErrorT
    -- * API
  , errorT
  , errorTE
  , lift
  , liftEither
  , liftEitherE
  , run
  , throw
  , throwE
  ) where

-- https://hackage.haskell.org/package/base
import Control.Exception (Exception(displayException))
import Data.Bifunctor (first)

-- https://hackage.haskell.org/package/transformers
import qualified Control.Monad.Trans.Except as Except
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import qualified Control.Monad.Trans.Class as Trans

------------------------------------------------------------------------------
-- $ErrorT

-- | Transformer used to manage errors: 'ExceptT' 'String'
type ErrorT = ExceptT String

------------------------------------------------------------------------------
-- $API

-- | Manage 'String' errors of an action
errorT
  :: Monad m
  => m (Either String a)
  -> ErrorT m a
errorT = ExceptT
{-# INLINE errorT #-}

------------------------------------------------------------------------------

-- | Manage 'Exception' errors of an action
errorTE
  :: (Exception e, Monad m)
  => m (Either e a)
  -> ErrorT m a
errorTE = ExceptT . fmap (first displayException)
{-# INLINE errorTE #-}

------------------------------------------------------------------------------

-- | Lift an action without errors
lift
  :: Monad m
  => m a
  -> ErrorT m a
lift = Trans.lift
{-# INLINE lift #-}

------------------------------------------------------------------------------

-- | Manage 'String' errors
liftEither
  :: Monad m
  => Either String a
  -> ErrorT m a
liftEither = ExceptT . pure
{-# INLINE liftEither #-}

------------------------------------------------------------------------------

-- | Manage 'Exception' errors
liftEitherE
  :: (Exception e, Monad m)
  => Either e a
  -> ErrorT m a
liftEitherE = ExceptT . pure . first displayException
{-# INLINE liftEitherE #-}

------------------------------------------------------------------------------

-- | Run the transformer
run
  :: Monad m
  => ErrorT m a
  -> m (Either String a)
run = Except.runExceptT
{-# INLINE run #-}

------------------------------------------------------------------------------

-- | Throw a 'String' error
throw
  :: Monad m
  => String
  -> ErrorT m a
throw = Except.throwE
{-# INLINE throw #-}

------------------------------------------------------------------------------

-- | Throw an 'Exception' error
throwE
  :: (Exception e, Monad m)
  => e
  -> ErrorT m a
throwE = Except.throwE . displayException
{-# INLINE throwE #-}
