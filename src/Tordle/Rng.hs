{-# LANGUAGE FlexibleContexts, LambdaCase, NamedFieldPuns #-}
module Tordle.Rng where

import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable
import System.Random.Stateful (StateGenM(..), StdGen, uniformRM)
import Tordle.Assets
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

randomWord
  :: MonadState StdGen m
  => Assets
  -> m String
randomWord
  = randomMember
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
