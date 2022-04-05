module Reactive.Banana.Extra where

import Control.Lens
import Data.Monoid (First)
import Reactive.Banana.Combinators hiding (First)


filterPrismE
  :: Getting (First a) s a  -- a Prism, Fold, or Traversal
  -> Event s
  -> Event a
filterPrismE optic
  = filterJust . fmap (preview optic)

statefulB
  :: MonadMoment m
  => a
  -> [Event (a -> a)]
  -> m (Behavior a)
statefulB a0
  = accumB a0
  . unions
  . reverse  -- so that the last function is applied last
