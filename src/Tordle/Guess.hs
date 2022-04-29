{-# LANGUAGE DeriveGeneric, FlexibleContexts, LambdaCase, NamedFieldPuns, OverloadedLabels #-}
module Tordle.Guess where

import Control.Lens
import Control.Monad.State
import Data.Generics.Labels ()
import Data.List (delete, nub)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Foreign.C.Types (CInt)
import GHC.Generics (Generic)
import Linear.V2 (V2(..))
import System.Random.Stateful (StdGen)
import Tordle.Assets
import Tordle.Model
import Tordle.Rng


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

data AnalyzedRow = AnalyzedRow
  { rowHadWilds
      :: Bool
  , rowCompletion
      :: String
  , rowExtraCompletions
      :: [String]
  , rowColoring
      :: Maybe Coloring
  , rowAction
      :: RowAction
  }
  deriving (Generic, Show)

data AnalyzedGuess = AnalyzedGuess
  { guessCompletion
      :: String
  , guessColoring
      :: Coloring
  }
  deriving (Generic, Show)

analyzeCompletedRow
  :: MonadState StdGen m
  => Assets
  -> String
  -> Map CInt Char
  -> Board
  -> CInt
  -> m AnalyzedRow
analyzeCompletedRow assets correctWord knownLetters board y = do
  let getLabel x = board ^?! ix (V2 x y) . #blockLabel
  let labels = map getLabel xCoordinates
  let hadWilds = has (each . #_Wild) labels
  let improvedLabels = improveLabels labels
  let generateCompletion = do
        randomCompatibleWord assets improvedLabels >>= \case
          Just completion -> do
            pure completion
          Nothing -> do
            randomCompatibleWord assets labels >>= \case
              Just completion -> do
                pure completion
              Nothing -> do
                randomCompatibleGibberish labels
  completion <- generateCompletion
  extraCompletions <- if hadWilds
    then nub <$> replicateM 8 generateCompletion
    else pure []
  if isRealWord assets completion
    then do
      pure $ AnalyzedRow
        { rowHadWilds
            = hadWilds
        , rowCompletion
            = completion
        , rowExtraCompletions
            = extraCompletions
        , rowColoring
            = Just $ analyzeGuess correctWord completion
        , rowAction
            = MoveRowToBottom
        }
    else do
      pure $ AnalyzedRow
        { rowHadWilds
            = hadWilds
        , rowCompletion
            = completion
        , rowExtraCompletions
            = extraCompletions
        , rowColoring
            = Nothing
        , rowAction
            = DeleteRow
        }
  where
    improveLabels
      :: [Label]
      -> [Label]
    improveLabels
      = iover itraversed $ \i label
     -> case (label, Map.lookup (fromIntegral i) knownLetters) of
          (Wild, Just knownLetter)
            -> Letter knownLetter
          _
            -> label

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

maybeStatusGuess
  :: BlockStatus
  -> Maybe GuessResult
maybeStatusGuess CorrectSpot
  = Just Green
maybeStatusGuess WrongSpot
  = Just Yellow
maybeStatusGuess NotInWord
  = Just Grey
maybeStatusGuess _
  = Nothing
