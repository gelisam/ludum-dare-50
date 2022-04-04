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
