module Tordle.Dir where

import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))


data Dir
  = N | E | W | S
  deriving (Bounded, Enum, Eq, Ord, Show)

dirs
  :: [Dir]
dirs
  = [minBound..maxBound]

towards
  :: Dir
  -> V2 CInt
towards N
  = V2 0 (-1)
towards E
  = V2 1 0
towards W
  = V2 (-1) 0
towards S
  = V2 0 1
