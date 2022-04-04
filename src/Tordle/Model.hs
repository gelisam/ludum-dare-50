module Tordle.Model where

import Data.Map (Map)
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))


bOARD_SIZE
  :: V2 CInt
bOARD_SIZE
  = V2 5 6


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
