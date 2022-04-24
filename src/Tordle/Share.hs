{-# LANGUAGE ImportQualifiedPost, NamedFieldPuns #-}
module Tordle.Share where

import Control.Lens
import Data.Map qualified as Map
import Data.Foldable
import Data.Traversable
import Linear.V2 (V2(..), _y)
import Tordle.Guess
import Tordle.Model


unicodeBoard
  :: Board
  -> [String]
unicodeBoard board
  = [ [ case color of
          Green
            -> '🟩'
          Yellow
            -> '🟨'
          Grey
            -> '⬜'
      | color <- coloring
      ]
    | y <- [0 .. (fULL_BOARD_SIZE^._y - 1)]
    , Just blocks <- [for xCoordinates $ \x -> Map.lookup (V2 x y) board]
    , Just coloring <- [for blocks $ \(Block {blockStatus}) -> maybeStatusGuess blockStatus]
    ]

-- Tordle 5/12
-- 
-- ⬜⬜⬜⬜⬜
-- ⬜⬜⬜⬜⬜
-- 🟩⬜🟨⬜🟨
-- 🟩⬜🟨⬜🟩
-- 🟩🟩🟩🟩🟩
printSharingText
  :: Board
  -> IO ()
printSharingText board = do
  putStrLn "Congratulations! Here's your guesses shape if you want to share it."
  putStrLn ""
  putStrLn $ "Tordle " ++ show (length unicodeLines) ++ "/" ++ show (mAIN_BOARD_SIZE^._y)
  putStrLn ""
  for_ unicodeLines $ \unicodeLine -> do
    putStrLn unicodeLine
  putStrLn ""
  putStrLn "---"
  where
    unicodeLines = unicodeBoard board
