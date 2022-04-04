module Tordle.Model where

import Data.Map (Map)
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))


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


data BlockStatus
  = Falling
  | InIncompleteWord
  | NotInWord
  | WrongSpot
  | CorrectSpot
  deriving Show

data Label
  = Letter Char
  | Wild
  deriving Show

data Block = Block
  { blockLabel
      :: Label
  , blockStatus
      :: BlockStatus
  }
  deriving Show

type Board
  = Map (V2 CInt) Block

data World = World
  { worldBoard
      :: Board
  }
