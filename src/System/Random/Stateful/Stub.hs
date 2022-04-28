{-# LANGUAGE FlexibleContexts #-}
module System.Random.Stateful.Stub
  ( StdGen
  , module System.Random.Stateful.Stub
  ) where

import Control.Monad.State
import System.Random (StdGen, split)


data StateGenM
  = StateGenM

splitGenM
  :: MonadState StdGen m
  => StateGenM
  -> m StdGen
splitGenM _ = do
  g <- get
  let (g1, g2) = split g
  put g2
  pure g1

class UniformRange a where
  uniformRM
    :: MonadState StdGen m
    => (a, a)
    -> StateGenM
    -> m a 

instance UniformRange Int where
  uniformRM _ _ = do
    pure 0
