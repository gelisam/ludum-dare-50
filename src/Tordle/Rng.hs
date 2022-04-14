{-# LANGUAGE ImportQualifiedPost, LambdaCase #-}
module Tordle.Rng where

import Control.Monad.Trans.State
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable
import System.Random.Stateful (StateGenM(..), StdGen, uniformRM)
import Tordle.Model
import Tordle.Tetromino


randomElement
  :: [a]
  -> State StdGen a
randomElement xs = do
  i <- uniformRM (0, length xs - 1) StateGenM
  pure (xs !! i)

randomLetter
  :: Set Char
  -> State StdGen Char
randomLetter
  = randomElement
  . Set.toList

randomOneSidedTetromino
  :: Set Char
  -> State StdGen (OneSidedTetromino Label)
randomOneSidedTetromino letters = do
  freeTetromino <- randomElement freeTetrominos
  labelledTetromino <- for freeTetromino $ \case
    Unlabelled -> do
      pure Wild
    Labelled -> do
      letter <- randomLetter letters
      pure $ Letter letter
  pure $ mkOneSidedTetromino labelledTetromino
