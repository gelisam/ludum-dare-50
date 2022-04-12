{-# LANGUAGE ImportQualifiedPost #-}
-- | Just for kicks, let's generate the tetrominoes from first principles instead of hardcoding them
module Tordle.Polynomino where

import Control.Lens
import Data.Foldable
import Data.Maybe qualified as Unsafe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..), _x, _y)
import Tordle.Dir


-- ....   ..##    ####
-- .##.   ..#.    ...#
-- .#.. = ..#. /= ....
-- .#..   ....    ....
newtype FixedPolynomino = FixedPolynomino
  { unFixedPolynomino
      :: Set (V2 CInt)
  }
  deriving (Eq, Ord, Show)

mkFixedPolynomino
  :: Set (V2 CInt)
  -> FixedPolynomino
mkFixedPolynomino positions
  = FixedPolynomino
  $ Set.map (subtract $ V2 minX minY)
  $ positions
  where
    minX = Unsafe.fromJust $ minimumOf (folded . _x) positions
    minY = Unsafe.fromJust $ minimumOf (folded . _y) positions

transformFixedPolynomino
  :: (V2 CInt -> V2 CInt)
  -> FixedPolynomino
  -> FixedPolynomino
transformFixedPolynomino f
  = mkFixedPolynomino
  . Set.map f
  . unFixedPolynomino

printFixedPolynomino
  :: FixedPolynomino
  -> IO ()
printFixedPolynomino (FixedPolynomino positions) = do
  for_ [minY..maxY] $ \y -> do
    for_ [minX..maxX] $ \x -> do
      putStr $ if V2 x y `Set.member` positions then "#" else "."
    putStrLn ""
  where
    minX = Unsafe.fromJust $ minimumOf (folded . _x) positions
    maxX = Unsafe.fromJust $ maximumOf (folded . _x) positions
    minY = Unsafe.fromJust $ minimumOf (folded . _y) positions
    maxY = Unsafe.fromJust $ maximumOf (folded . _y) positions

fixedMonomino
  :: FixedPolynomino
fixedMonomino
  = mkFixedPolynomino
  $ Set.singleton 0

fixedPolynominoExtensions
  :: FixedPolynomino
  -> Set FixedPolynomino
fixedPolynominoExtensions (FixedPolynomino positions)
  = Set.fromList
  $ fmap mkFixedPolynomino
  $ [ Set.insert pos' positions
    | pos <- Set.toList positions
    , dir <- dirs
    , let pos' = pos + towards dir
    , not $ pos' `Set.member` positions
    ]

-- of size 1, then 2, then 3...
fixedPolynominoes
  :: [Set FixedPolynomino]
fixedPolynominoes
  = Set.singleton fixedMonomino
  : fmap (foldMap fixedPolynominoExtensions) fixedPolynominoes



-- ....   ..##   ####    ...#
-- .##.   ..#.   ...#    ####
-- .#.. = ..#. = .... /= ....
-- .#..   ....   ....    ....
newtype OneSidedPolynomino = OneSidedPolynomino
  { unOneSidedPolynomino
      :: Set FixedPolynomino  -- set of rotations
  }
  deriving (Eq, Ord, Show)

mkOneSidedPolynomino
  :: FixedPolynomino
  -> OneSidedPolynomino
mkOneSidedPolynomino
  = OneSidedPolynomino
  . Set.fromList
  . fmap mkFixedPolynomino
  . take 4
  . iterate (Set.map (\(V2 x y) -> V2 y (-x)))
  . unFixedPolynomino

pickExampleRotation
  :: OneSidedPolynomino
  -> FixedPolynomino
pickExampleRotation
  = head
  . Set.toList
  . unOneSidedPolynomino

transformOneSidedPolynomino
  :: (V2 CInt -> V2 CInt)
  -> OneSidedPolynomino
  -> OneSidedPolynomino
transformOneSidedPolynomino f
  = mkOneSidedPolynomino
  . transformFixedPolynomino f
  . pickExampleRotation

printOneSidedPolynomino
  :: OneSidedPolynomino
  -> IO ()
printOneSidedPolynomino
  = printFixedPolynomino
  . pickExampleRotation

oneSidedMonomino
  :: OneSidedPolynomino
oneSidedMonomino
  = mkOneSidedPolynomino fixedMonomino

oneSidedPolynominoExtensions
  :: OneSidedPolynomino
  -> Set OneSidedPolynomino
oneSidedPolynominoExtensions
  = Set.map mkOneSidedPolynomino
  . fixedPolynominoExtensions
  . pickExampleRotation

-- of size 1, then 2, then 3...
oneSidedPolynominoes
  :: [Set OneSidedPolynomino]
oneSidedPolynominoes
  = Set.singleton oneSidedMonomino
  : fmap (foldMap oneSidedPolynominoExtensions) oneSidedPolynominoes


-- ....   ..##   .###   ...#    ...#
-- .##.   ..#.   ...#   .###    ..##
-- .#.. = ..#. = .... = .... /= ..#.
-- .#..   ....   ....   ....    ....
newtype FreePolynomino = FreePolynomino
  { unFreePolymino :: Set OneSidedPolynomino  -- set of symmetries
  }
  deriving (Eq, Ord, Show)

mkFreePolynomino
  :: OneSidedPolynomino
  -> FreePolynomino
mkFreePolynomino poly
  = FreePolynomino
  $ Set.fromList
      [ poly
      , transformOneSidedPolynomino (over _x negate) poly
      , transformOneSidedPolynomino (over _y negate) poly
      ]

pickExampleSymmetry
  :: FreePolynomino
  -> OneSidedPolynomino
pickExampleSymmetry
  = head
  . Set.toList
  . unFreePolymino

transformFreePolynomino
  :: (V2 CInt -> V2 CInt)
  -> FreePolynomino
  -> FreePolynomino
transformFreePolynomino f
  = mkFreePolynomino
  . transformOneSidedPolynomino f
  . pickExampleSymmetry

printFreePolynomino
  :: FreePolynomino
  -> IO ()
printFreePolynomino
  = printOneSidedPolynomino
  . pickExampleSymmetry

freeMonomino
  :: FreePolynomino
freeMonomino
  = mkFreePolynomino oneSidedMonomino

freePolynominoExtensions
  :: FreePolynomino
  -> Set FreePolynomino
freePolynominoExtensions
  = Set.map mkFreePolynomino
  . oneSidedPolynominoExtensions
  . pickExampleSymmetry

-- of size 1, then 2, then 3...
freePolynominoes
  :: [Set FreePolynomino]
freePolynominoes
  = Set.singleton freeMonomino
  : fmap (foldMap freePolynominoExtensions) freePolynominoes


-- |
-- >>> test
-- #
-- #
-- #
-- #
-- ---
-- ##
-- #.
-- #.
-- ---
-- #.
-- ##
-- #.
-- ---
-- ##
-- ##
-- ---
-- #.
-- ##
-- .#
-- ---
test :: IO ()
test = do
  forOf_ folded (freePolynominoes !! 3) $ \poly -> do
    printFreePolynomino poly
    putStrLn "---"
