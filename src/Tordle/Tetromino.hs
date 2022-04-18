{-# LANGUAGE DeriveFoldable, DeriveGeneric, DeriveTraversable, ImportQualifiedPost #-}
module Tordle.Tetromino where

import Control.Lens hiding (mapping)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.Maybe qualified as Unsafe (fromJust)
import Data.Map (Map)
import Data.Map qualified as Map
import Foreign.C.Types (CInt)
import GHC.Generics (Generic)
import Linear.Extra
import Linear.V2 (V2(..), _x, _y)


fixedTetrominoShapes
  :: [[String]]
fixedTetrominoShapes
  = [ [ "A##A"
      ]
    , [ "A#."
      , ".#A"
      ]
    , [ ".#A"
      , "A#."
      ]
    , [ "A##"
      , "..A"
      ]
    , [ "..A"
      , "A##"
      ]
    , [ "A#"
      , "#A"
      ]
    , [ "A#A"
      , ".A."
      ]
    ]


data BlockType
  = Labelled
  | Unlabelled
  deriving (Eq, Generic, Ord, Show)

newtype FixedTetromino a = FixedTetromino
  { unFixedTetromino
      :: Map (V2 CInt) a
  }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

mkFixedTetromino
  :: Map (V2 CInt) a
  -> FixedTetromino a
mkFixedTetromino positions
  = FixedTetromino
  $ Map.mapKeys (subtract center)
  $ positions
  where
    maxX = Unsafe.fromJust $ maximumOf (to Map.keys . folded . _x) positions
    maxY = Unsafe.fromJust $ maximumOf (to Map.keys . folded . _y) positions
    center = half (V2 maxX maxY)

printFixedTetromino
  :: FixedTetromino BlockType
  -> IO ()
printFixedTetromino (FixedTetromino positions) = do
  for_ [minY-1..maxY+1] $ \y -> do
    for_ [minX-1..maxX+1] $ \x -> do
      case Map.lookup (V2 x y) positions of
        Nothing | x == 0 && y == 0 -> do
          putStr "+"
        Nothing | x == 0 -> do
          putStr "|"
        Nothing | y == 0 -> do
          putStr "-"
        Nothing -> do
          putStr "."
        Just Unlabelled -> do
          putStr "#"
        Just Labelled -> do
          putStr "A"
    putStrLn ""
  where
    minX = Unsafe.fromJust $ minimumOf (to Map.keys . folded . _x) positions
    maxX = Unsafe.fromJust $ maximumOf (to Map.keys . folded . _x) positions
    minY = Unsafe.fromJust $ minimumOf (to Map.keys . folded . _y) positions
    maxY = Unsafe.fromJust $ maximumOf (to Map.keys . folded . _y) positions

parseFixedTetromino
  :: [String]
  -> Maybe (FixedTetromino BlockType)
parseFixedTetromino rows = do
  mapping <- execWriterT $ do
    for_ (zip [0..] rows) $ \(y,row) -> do
      for_ (zip [0..] row) $ \(x,c) -> do
        case c of
          '.' -> do
            pure ()
          'A' -> do
            tell $ Map.singleton (V2 x y) Labelled
          '#' -> do
            tell $ Map.singleton (V2 x y) Unlabelled
          _ -> do
            lift Nothing
  pure $ mkFixedTetromino mapping

fixedTetrominos
  :: [FixedTetromino BlockType]
fixedTetrominos
  = [ tetromino
    | tetrominoShape <- fixedTetrominoShapes
    , Just tetromino <- [parseFixedTetromino tetrominoShape]
    ]


newtype OneSidedTetromino a = OneSidedTetromino
  { unOneSidedTetromino
      :: [FixedTetromino a]  -- infinite rotations
  }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

mkOneSidedTetromino
  :: Ord a
  => FixedTetromino a
  -> OneSidedTetromino a
mkOneSidedTetromino
  = OneSidedTetromino
  . fmap mkFixedTetromino
  . iterate (Map.mapKeys (\(V2 x y) -> V2 y (-x)))
  . unFixedTetromino

runOneSidedTetromino
  :: OneSidedTetromino a
  -> FixedTetromino a
runOneSidedTetromino
  = head
  . unOneSidedTetromino

rotateOneSidedTetromino
  :: OneSidedTetromino a
  -> OneSidedTetromino a
rotateOneSidedTetromino
  = OneSidedTetromino
  . tail
  . unOneSidedTetromino

printOneSidedTetromino
  :: OneSidedTetromino BlockType
  -> IO ()
printOneSidedTetromino (OneSidedTetromino rotations) = do
  for_ (take 4 rotations) $ \tetromino -> do
    printFixedTetromino tetromino
    putStrLn ""

oneSidedTetrominos
  :: [OneSidedTetromino BlockType]
oneSidedTetrominos
  = fmap mkOneSidedTetromino fixedTetrominos


test
  :: IO ()
test = do
  for_ oneSidedTetrominos $ \tetromino -> do
    printOneSidedTetromino tetromino
    putStrLn ""
    putStrLn ""
