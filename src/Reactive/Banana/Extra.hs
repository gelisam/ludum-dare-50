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

newtype EventFun a b = EventFun
  { unChange
      :: (Event a -> Event b)
  }

withBehaviour
  :: Behavior b
  -> EventFun (a, b) c
  -> EventFun a c
withBehaviour bB (EventFun f)
  = EventFun $ \aE
 -> f (flip (,) <$> bB <@> aE)


onEvent
  :: Event a
  -> EventFun a b
  -> Event b
onEvent aE (EventFun f)
  = f aE

changeValue
  :: ((a, b) -> b)
  -> EventFun a (b -> b)
changeValue f
  = EventFun $ fmap (curry f)

setValue
  :: (a -> b)
  -> EventFun a (b -> b)
setValue f
  = changeValue $ \(a,_)
 -> f a

changeState
  :: (a -> State s ())
  -> EventFun a (s -> s)
changeState body
  = changeValue $ \(a,s0)
 -> flip execState s0
  $ body a

changeStateRandomly
  :: RandomGen g
  => (a -> StateT g (State s) ())
  -> EventFun a ((g,s) -> (g,s))
changeStateRandomly body
  = changeValue $ \(a,(g0,s0))
 -> flip runState s0
  $ flip execStateT g0
  $ body a

setValueRandomly
  :: RandomGen g
  => (a -> State g b)
  -> EventFun a ((g,b) -> (g,b))
setValueRandomly body
  = changeValue $ \(a,(g0,_))
 -> swap
  $ flip runState g0
  $ body a


givenEvent
  :: Event a
  -> EventFun a (Maybe a)
  -> Event a
givenEvent aE (EventFun f)
  = filterJust (f aE)

maybeKeepIt
  :: (a -> Maybe b)
  -> EventFun a (Maybe b)
maybeKeepIt f
  = EventFun (fmap f)
