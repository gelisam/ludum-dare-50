{-# LANGUAGE DeriveGeneric, ImportQualifiedPost, OverloadedLabels, RecordWildCards #-}
module Tordle.Model where

import Control.Lens
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Map qualified as Map
import Foreign.C.Types (CInt)
import GHC.Generics (Generic)
import Linear.V2 (V2(..), _x, _y)


mAIN_BOARD_SIZE
  :: V2 CInt
mAIN_BOARD_SIZE
  = V2 5 6

aBOVE_BOARD_BUFFER
  :: CInt
aBOVE_BOARD_BUFFER
  = 4

fULL_BOARD_SIZE
  :: V2 CInt
fULL_BOARD_SIZE
  = mAIN_BOARD_SIZE + V2 0 aBOVE_BOARD_BUFFER


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
  deriving (Generic, Show)

data Label
  = Letter Char
  | Wild
  deriving (Generic, Show)

data Block = Block
  { blockLabel
      :: Label
  , blockStatus
      :: BlockStatus
  }
  deriving (Generic, Show)

type Board
  = Map (V2 CInt) Block

data Piece = Piece
  { pieceBlocks
      :: Map (V2 CInt) Label
  , piecePos
      :: V2 CInt
  }
  deriving (Generic, Show)

rotateLeft
  :: Map (V2 CInt) Label
  -> Map (V2 CInt) Label
rotateLeft
  = Map.mapKeys (\(V2 x y) -> V2 y (-x))

rotateRight
  :: Map (V2 CInt) Label
  -> Map (V2 CInt) Label
rotateRight
  = Map.mapKeys (\(V2 x y) -> V2 (-y) x)

renderPiece
  :: Piece
  -> Board
renderPiece (Piece {..})
  = Map.fromList
      [ (piecePos + pos, Block label Falling)
      | (pos, label) <- Map.toList pieceBlocks
      ]

data World = World
  { worldBoard
      :: Board
  , worldCurrentPiece
      :: Piece
  }
  deriving (Generic, Show)
