{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Reactive.Banana.Extra where

import Control.Lens
import Control.Monad.Trans.State
import Data.Monoid qualified as Monoid
import Data.Tuple (swap)
import Reactive.Banana.Combinators hiding (First)
import System.Random.Stateful


filterPrismE
  :: Getting (Monoid.First a) s a  -- a Prism, Fold, or Traversal
  -> Event s
  -> Event a
filterPrismE optic
  = filterJust . fmap (preview optic)


changingB
  :: MonadMoment m
  => a
  -> [Event (a -> a)]
  -> m (Behavior a)
changingB a0
  = accumB a0
  . unions
  . reverse  -- so that the last function is applied last

newtype Change a b = Change
  { unChange
      :: (Event a -> Event b)
  }

onEvent
  :: Event a
  -> Change a b
  -> Event b
onEvent aE (Change f)
  = f aE

withBehaviour
  :: Behavior b
  -> Change (a, b) c
  -> Change a c
withBehaviour bB (Change f)
  = Change $ \aE
 -> f (flip (,) <$> bB <@> aE)

changeValue
  :: ((a, b) -> b)
  -> Change a (b -> b)
changeValue f
  = Change $ fmap (curry f)

setValue
  :: (a -> b)
  -> Change a (b -> b)
setValue f
  = changeValue $ \(a,_)
 -> f a

changeState
  :: (a -> State s ())
  -> Change a (s -> s)
changeState body
  = changeValue $ \(a,s0)
 -> flip execState s0
  $ body a

changeStateRandomly
  :: RandomGen g
  => (a -> StateT g (State s) ())
  -> Change a ((g,s) -> (g,s))
changeStateRandomly body
  = changeValue $ \(a,(g0,s0))
 -> flip runState s0
  $ flip execStateT g0
  $ body a

setValueRandomly
  :: RandomGen g
  => (a -> State g b)
  -> Change a ((g,b) -> (g,b))
setValueRandomly body
  = changeValue $ \(a,(g0,_))
 -> swap
  $ flip runState g0
  $ body a
