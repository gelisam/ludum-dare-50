{-# LANGUAGE DeriveGeneric #-}
module Tordle.Guess where

import Data.List (delete)
import GHC.Generics (Generic)
import Tordle.Model


data GuessResult
  = Green
  | Yellow
  | Grey
  deriving (Eq, Generic, Ord, Show)

analyzeGuess
  :: [Char]  -- ^ correct word
  -> [Char]  -- ^ guess
  -> [GuessResult]
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
