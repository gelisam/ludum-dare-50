{-# LANGUAGE FlexibleContexts, LambdaCase, NamedFieldPuns #-}
module Tordle.Rng where

import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable
import System.Random.Stateful (StateGenM(..), StdGen, uniformRM)
import Tordle.Model


randomIndex
  :: MonadState StdGen m
  => [a]
  -> m Int
randomIndex xs = do
  uniformRM (0, length xs - 1) StateGenM

randomElement
  :: MonadState StdGen m
  => [a]
  -> m a
randomElement xs = do
  i <- randomIndex xs
  pure (xs !! i)

randomMember
  :: (MonadState StdGen m, Ord a)
  => Set a
  -> m a
randomMember xs = do
  i <- uniformRM (0, Set.size xs - 1) StateGenM
  pure (Set.elemAt i xs)

randomCompatibleGibberish
  :: MonadState StdGen m
  => [Label]
  -> m String
randomCompatibleGibberish labels = do
  for labels $ \label -> do
    case label of
      Letter letter -> do
        pure letter
      Wild -> do
        randomLetter (Set.fromList allLetters)

randomLetter
  :: MonadState StdGen m
  => Set Char
  -> m Char
randomLetter
  = randomElement
  . Set.toList
