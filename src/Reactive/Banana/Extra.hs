{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Reactive.Banana.Extra where

import Control.Lens
import Data.Monoid qualified as Monoid
import Data.Semigroup qualified as Semigroup
import Reactive.Banana.Combinators hiding (First)


filterPrismE
  :: Getting (Monoid.First a) s a  -- a Prism, Fold, or Traversal
  -> Event s
  -> Event a
filterPrismE optic
  = filterJust . fmap (preview optic)

filterBE
  :: Behavior (a -> Bool)
  -> Event a
  -> Event a
filterBE predB valueE
  = fmap snd
  $ filterE fst
  $ (\pred value -> (pred value, value)) <$> predB <@> valueE

accumsB
  :: MonadMoment m
  => a
  -> [Event (a -> a)]
  -> m (Behavior a)
accumsB a0
  = accumB a0
  . unions
  . reverse  -- so that the last function is applied last

steppersB
  :: MonadMoment m
  => a
  -> [Event a]
  -> m (Behavior a)
steppersB a0
  = stepper a0
  . fmap Semigroup.getFirst
  . mconcat
  . fmap (fmap Semigroup.First)
