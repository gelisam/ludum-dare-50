{-# LANGUAGE DeriveGeneric, OverloadedLabels, RankNTypes, RecordWildCards #-}
module Tordle.Model where

import Control.Lens
import Data.Generics.Labels ()
import Data.Map (Map)
import qualified Data.Map as Map
import Foreign.C.Types (CInt)
import GHC.Generics (Generic)
import Linear.V2 (V2(..), _x, _y)
import Tordle.Tetromino


mAIN_BOARD_SIZE
  :: V2 CInt
mAIN_BOARD_SIZE
  = V2 5 12

aBOVE_BOARD_BUFFER
  :: CInt
aBOVE_BOARD_BUFFER
  = 4

fULL_BOARD_SIZE
  :: V2 CInt
fULL_BOARD_SIZE
  = mAIN_BOARD_SIZE + V2 0 aBOVE_BOARD_BUFFER

pHANTOM_BOARD_SIZE
  :: V2 CInt
pHANTOM_BOARD_SIZE
  = fULL_BOARD_SIZE + V2 0 (aBOVE_BOARD_BUFFER - 2)


xCoordinates
  :: [CInt]
xCoordinates
  = [0 .. mAIN_BOARD_SIZE^._x - 1]

allLetters
  :: [Char]
allLetters
  = ['A'..'Z']


inBoardWidth
  :: CInt
  -> Bool
inBoardWidth x
  = x >= 0
 && x < mAIN_BOARD_SIZE^._x

inFullBoard
  :: V2 CInt
  -> Bool
inFullBoard (V2 x y)
  = inBoardWidth x
 && y >= 0
 && y < fULL_BOARD_SIZE^._y

inMainBoard
  :: V2 CInt
  -> Bool
inMainBoard (V2 x y)
  = inBoardWidth x
 && y >= aBOVE_BOARD_BUFFER
 && y < fULL_BOARD_SIZE^._y


data BlockStatus
  = Falling
  | InIncompleteWord
  | NotInWord
  | WrongSpot
  | CorrectSpot
  deriving (Eq, Generic, Ord, Show)

data Label
  = Letter Char
  | Wild
  deriving (Eq, Generic, Ord, Show)

data Block = Block
  { blockLabel
      :: Label
  , blockStatus
      :: BlockStatus
  , blockOffset
      :: V2 Double
  , blockZIndex
      :: CInt  -- ^ bigger values are drawn in front
  }
  deriving (Eq, Generic, Ord, Show)

type Board
  = Map (V2 CInt) Block

moveAllRows
  :: Ord k
  => Lens' k CInt
  -> Map CInt CInt  -- from/to; no duplicated to's, missing rows are deleted
  -> Map k a
  -> Map k a
moveAllRows yLens moves board
  = Map.fromList
      [ (set yLens y' k, block)
      | (k, block) <- Map.toList board
      , let y = view yLens k
      , Just y' <- [Map.lookup y moves]
      ]

animateAllRows
  :: Double
  -> Map CInt CInt  -- from/to; no duplicated to's, missing rows are deleted
  -> Board
  -> Board
animateAllRows t moves board
  = Map.fromList
      [ (V2 x y, block')
      | (V2 x y, block) <- Map.toList board
      , Just y' <- [Map.lookup y moves]
      , let t' = if y' > y then t*t else t  -- accelerate downward motions
      , let block' = set (#blockOffset . _y)
                         (t' * (fromIntegral y' - fromIntegral y))
                         block
      ]

resetAllOffsets
  :: Board
  -> Board
resetAllOffsets
  = set (each . #blockOffset) 0

data RowAction
  = DeleteRow
  | MoveRowToBottom
  deriving (Eq, Generic, Ord, Show)

rowActionDestinations
  :: Map CInt RowAction
  -> Map CInt CInt
rowActionDestinations rowActions
  = Map.fromList
  $ fst
  $ go aBOVE_BOARD_BUFFER []
  where
    maxY
      :: CInt
    maxY
      = fULL_BOARD_SIZE^._y - 1

    go
      :: CInt              -- row 'y' to consider next
      -> [CInt]            -- rows to be appended at the end
      -> ( [(CInt, CInt)]  -- destinations for rows 'y' and below
         , CInt            -- delta to apply to rows above 'y'
         )
    go y bottom
      | y > maxY
        = case bottom of
            []
              -> ([], 0)
            z:zs
              -> let (moves, dy) = go y zs
              in ((z, maxY + dy) : moves, dy-1)
      | otherwise
        = case Map.lookup y rowActions of
            Nothing
              -> let (moves, dy) = go (y+1) bottom
              in ((y, y + dy) : moves, dy)
            Just DeleteRow
              -> let (moves, dy) = go (y+1) bottom
              in (moves, dy+1)
            Just MoveRowToBottom
              -> let (moves, dy) = go (y+1) (bottom ++ [y])
              in (moves, dy+1)

performRowActions
  :: Ord k
  => Lens' k CInt
  -> Map CInt RowAction
  -> Map k a
  -> Map k a
performRowActions yLens rowActions
  = moveAllRows yLens
  $ rowActionDestinations rowActions

animateRowActions
  :: Map CInt RowAction
  -> Double
  -> Board
  -> Board
animateRowActions rowActions t
  = animateAllRows t
  $ rowActionDestinations rowActions

data Piece = Piece
  { pieceBlocks
      :: OneSidedTetromino Label
  , piecePos
      :: V2 CInt
  }
  deriving (Eq, Generic, Ord, Show)

rotateLeft
  :: OneSidedTetromino a
  -> OneSidedTetromino a
rotateLeft
  = rotateOneSidedTetromino

rotateRight
  :: OneSidedTetromino a
  -> OneSidedTetromino a
rotateRight
  = rotateOneSidedTetromino
  . rotateOneSidedTetromino
  . rotateOneSidedTetromino

renderPiece
  :: Piece
  -> Board
renderPiece (Piece {..})
  = Map.fromList
      [ (piecePos + pos, Block label Falling 0 0)
      | (pos, label) <- Map.toList $ unFixedTetromino $ runOneSidedTetromino pieceBlocks
      ]

data WorldStatus
  = Playing
  | GameOver
  | Win
  deriving (Eq, Generic, Ord, Show)

data GuessResult
  = Grey
  | Yellow
  | Green
  deriving (Eq, Generic, Ord, Show)

type AlphabetColoring
  = Map Char GuessResult

data HelpText
  = HelpGuessLetter
  | HelpPlaceBlock
  | HelpPlayAgain
  | HelpChangeShape
  | HelpNotAWord
  | HelpSolution
  deriving (Eq, Generic, Ord, Show)

data Sound
  = SoundMove
  | SoundLand
  | SoundNotAWord
  | SoundGreyLetter
  | SoundYellowLetter
  | SoundGreenLetter CInt
  | SoundGameOver
  deriving (Eq, Generic, Ord, Show)

data World = World
  { worldStatus
      :: WorldStatus
  , worldMaybeSolution
      :: Maybe String
  , worldMaybeHelpText
      :: Maybe HelpText
  , worldAlphabetColoring
      :: AlphabetColoring
  , worldBoard
      :: Board
  , worldCurrentPiece
      :: Piece
  }
  deriving (Eq, Generic, Ord, Show)
