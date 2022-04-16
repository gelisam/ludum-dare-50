{-# LANGUAGE FlexibleContexts, ImportQualifiedPost, LambdaCase, NamedFieldPuns #-}
module Tordle.Rng where

import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable
import System.Random.Stateful (StateGenM(..), StdGen, uniformRM)
import Tordle.Assets
import Tordle.Model
import Tordle.Tetromino


randomElement
  :: MonadState StdGen m
  => [a]
  -> m a
randomElement xs = do
  i <- uniformRM (0, length xs - 1) StateGenM
  pure (xs !! i)

randomWord
  :: MonadState StdGen m
  => Assets
  -> m String
randomWord
  = randomElement
  . assetsCommonWords

randomCompatibleWord
  :: MonadState StdGen m
  => Assets
  -> [Label]
  -> m (Maybe String)
randomCompatibleWord (Assets {assetsAllWords}) labels = do
  compatibleWords <- execWriterT $ do
    for_ assetsAllWords $ \potentialWord -> do
      when (isCompatibleWord potentialWord) $ do
        tell [potentialWord]
  case compatibleWords of
    [] -> do
      pure Nothing
    _ -> do
      Just <$> randomElement compatibleWords
  where
    isCompatibleLetter
      :: Label
      -> Char
      -> Bool
    isCompatibleLetter Wild _
      = True
    isCompatibleLetter (Letter c1) c2
      = c1 == c2

    isCompatibleWord
      :: String
      -> Bool
    isCompatibleWord
      = all (uncurry isCompatibleLetter)
      . zip labels

randomCompletion
  :: MonadState StdGen m
  => Assets
  -> [Label]
  -> m String
randomCompletion assets labels = do
  maybeCompletion <- randomCompatibleWord assets labels
  case maybeCompletion of
    Just completion -> do
      pure completion
    Nothing -> do
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
