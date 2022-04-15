{-# LANGUAGE FlexibleContexts, ImportQualifiedPost, LambdaCase #-}
module Tordle.Rng where

import Control.Monad.State
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable
import System.Random.Stateful (StateGenM(..), StdGen, uniformRM)
import Tordle.Model
import Tordle.Tetromino


randomElement
  :: MonadState StdGen m
  => [a]
  -> m a
randomElement xs = do
  i <- uniformRM (0, length xs - 1) StateGenM
  pure (xs !! i)

randomLetter
  :: MonadState StdGen m
  => Set Char
  -> m Char
randomLetter
  = randomElement
  . Set.toList

randomOneSidedTetromino
  :: MonadState StdGen m
  => Set Char
  -> m (OneSidedTetromino Label)
randomOneSidedTetromino letters = do
  freeTetromino <- randomElement freeTetrominos
  labelledTetromino <- for freeTetromino $ \case
    Unlabelled -> do
      pure Wild
    Labelled -> do
      letter <- randomLetter letters
      pure $ Letter letter
  pure $ mkOneSidedTetromino labelledTetromino
