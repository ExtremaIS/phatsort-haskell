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
--
-- @since 0.5.0.0
type ErrorT = ExceptT String

------------------------------------------------------------------------------
-- $API

-- | Manage 'String' errors of an action
--
-- @since 0.5.0.0
errorT
  :: Monad m
  => m (Either String a)
  -> ErrorT m a
errorT = ExceptT
{-# INLINE errorT #-}

------------------------------------------------------------------------------

-- | Manage 'Exception' errors of an action
--
-- @since 0.5.0.0
errorTE
  :: (Exception e, Monad m)
  => m (Either e a)
  -> ErrorT m a
errorTE = ExceptT . fmap (first displayException)
{-# INLINE errorTE #-}

------------------------------------------------------------------------------

-- | Lift an action without errors
--
-- @since 0.5.0.0
lift
  :: Monad m
  => m a
  -> ErrorT m a
lift = Trans.lift
{-# INLINE lift #-}

------------------------------------------------------------------------------

-- | Manage 'String' errors
--
-- @since 0.5.0.0
liftEither
  :: Monad m
  => Either String a
  -> ErrorT m a
liftEither = ExceptT . pure
{-# INLINE liftEither #-}

------------------------------------------------------------------------------

-- | Manage 'Exception' errors
--
-- @since 0.5.0.0
liftEitherE
  :: (Exception e, Monad m)
  => Either e a
  -> ErrorT m a
liftEitherE = ExceptT . pure . first displayException
{-# INLINE liftEitherE #-}

------------------------------------------------------------------------------

-- | Run the transformer
--
-- @since 0.5.0.0
run
  :: Monad m
  => ErrorT m a
  -> m (Either String a)
run = Except.runExceptT
{-# INLINE run #-}

------------------------------------------------------------------------------

-- | Throw a 'String' error
--
-- @since 0.5.0.0
throw
  :: Monad m
  => String
  -> ErrorT m a
throw = Except.throwE
{-# INLINE throw #-}

------------------------------------------------------------------------------

-- | Throw an 'Exception' error
--
-- @since 0.5.0.0
throwE
  :: (Exception e, Monad m)
  => e
  -> ErrorT m a
throwE = Except.throwE . displayException
{-# INLINE throwE #-}
