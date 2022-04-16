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
  -> [Maybe Char]
  -> m (Maybe String)
randomCompatibleWord (Assets {assetsAllWords}) maybeLetters = do
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
      :: Maybe Char
      -> Char
      -> Bool
    isCompatibleLetter Nothing _
      = True
    isCompatibleLetter (Just c1) c2
      = c1 == c2

    isCompatibleWord
      :: String
      -> Bool
    isCompatibleWord
      = all (uncurry isCompatibleLetter)
      . zip maybeLetters

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
