{-# LANGUAGE ImportQualifiedPost, NamedFieldPuns #-}
module Tordle.Guess where

import Data.List (delete)
import Data.Set qualified as Set
import Tordle.Assets
import Tordle.Model


type Coloring
  = [GuessResult]

isRealWord
  :: Assets
  -> String
  -> Bool
isRealWord (Assets {assetsAllWords}) guess
  = guess `Set.member` assetsAllWords

analyzeGuess
  :: [Char]  -- ^ correct word
  -> [Char]  -- ^ guess
  -> Coloring
analyzeGuess correct guess
  = go (zip guess isGreen) possiblyYellowLetters
  where
    isGreen
      :: [Bool]
    isGreen
      = zipWith (==) correct guess

    possiblyYellowLetters
      :: [Char]
    possiblyYellowLetters
      = fmap snd
      $ filter (not . fst)
      $ zip isGreen correct

    go 
      :: [(Char, Bool)]  -- ^ @zip guess isGreen@ suffix
      -> [Char]          -- ^ 'possiblyYellowLetters' subset
      -> [GuessResult]
    go [] _
      = []
    go ((_,True):xs) ys
      = Green
      : go xs ys
    go ((x,False):xs) ys
      | x `elem` ys
        = Yellow
        : go xs (delete x ys)
      | otherwise
        = Grey
        : go xs ys

guessStatus
  :: GuessResult
  -> BlockStatus
guessStatus Green
  = CorrectSpot
guessStatus Yellow
  = WrongSpot
guessStatus Grey
  = NotInWord

maybeGuessStatus
  :: Maybe GuessResult
  -> BlockStatus
maybeGuessStatus Nothing
  = InIncompleteWord
maybeGuessStatus (Just guessResult)
  = guessStatus guessResult
