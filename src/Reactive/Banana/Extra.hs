{-# LANGUAGE DeriveGeneric, FlexibleContexts, ImportQualifiedPost, OverloadedLabels, RankNTypes #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Reactive.Banana.Extra where

import Control.Lens
import Control.Monad.State
import Data.Generics.Labels ()
import Data.Monoid qualified as Monoid
import GHC.Generics (Generic)
import Reactive.Banana.Combinators hiding (First)
import System.Random.Stateful


filterPrismE
  :: Getting (Monoid.First a) s a  -- a Prism, Fold, or Traversal
  -> Event s
  -> Event a
filterPrismE optic
  = filterJust . fmap (preview optic)


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


changingB
  :: MonadMoment m
  => a
  -> [Event (a -> a)]
  -> m (Behavior a)
changingB a0
  = accumB a0
  . unions
  . reverse  -- so that the last function is applied last

changingOfB
  :: MonadMoment m
  => Getting a aa a
  -> aa
  -> [Event (aa -> aa)]
  -> m (Behavior a)
changingOfB aa2a aa0 events = do
  aaB <- changingB aa0 events
  pure $ fmap (view aa2a) aaB

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

changeValueOf
  :: Setter' bb b
  -> ((a, b) -> b)
  -> EventFun a (bb -> bb)
changeValueOf bb2b f
  = changeValue $ \(a,bb)
 -> over bb2b (curry f a) bb

setValue
  :: (a -> b)
  -> EventFun a (b -> b)
setValue f
  = changeValue $ \(a,_)
 -> f a

setValueOf
  :: Setter' bb b
  -> (a -> b)
  -> EventFun a (bb -> bb)
setValueOf bb2b f
  = changeValueOf bb2b $ \(a,_)
 -> f a

changeState
  :: (a -> State s ())
  -> EventFun a (s -> s)
changeState body
  = changeValue $ \(a,s0)
 -> flip execState s0
  $ body a

changeStateOf
  :: Lens' ss s
  -> (a -> State s ())
  -> EventFun a (ss -> ss)
changeStateOf ss2s body
  = changeState $ \a -> do
      zoom ss2s $ do
        body a


data ValueAndRng a = ValueAndRng
  { theValue
      :: a
  , theRng
      :: StdGen
  }
  deriving (Generic, Show)

changingRandomlyB
  :: ( MonadMoment m
     , MonadState StdGen m
     )
  => a
  -> [Event (ValueAndRng a -> ValueAndRng a)]
  -> m (Behavior a)
changingRandomlyB a0 events = do
  rng0 <- splitGenM StateGenM
  changingOfB #theValue (ValueAndRng a0 rng0) events

setValueRandomly
  :: (a -> State StdGen b)
  -> EventFun a (ValueAndRng b -> ValueAndRng b)
setValueRandomly body
  = changeState $ \a -> do
      b <- zoom #theRng $ do
        body a
      #theValue .= b


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
