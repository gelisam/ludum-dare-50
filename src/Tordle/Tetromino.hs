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


freeTetrominoShapes
  :: [[String]]
freeTetrominoShapes
  = [ [ "A##A"
      ]
    , [ "A#."
      , ".#A"
      ]
    , [ "A##"
      , "..A"
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

newtype FreeTetromino a = FreeTetromino
  { unFreeTetromino
      :: Map (V2 CInt) a
  }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

mkFreeTetromino
  :: Map (V2 CInt) a
  -> FreeTetromino a
mkFreeTetromino positions
  = FreeTetromino
  $ Map.mapKeys (subtract center)
  $ positions
  where
    maxX = Unsafe.fromJust $ maximumOf (to Map.keys . folded . _x) positions
    maxY = Unsafe.fromJust $ maximumOf (to Map.keys . folded . _y) positions
    center = half (V2 maxX maxY)

printFreeTetromino
  :: FreeTetromino BlockType
  -> IO ()
printFreeTetromino (FreeTetromino positions) = do
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

parseFreeTetromino
  :: [String]
  -> Maybe (FreeTetromino BlockType)
parseFreeTetromino rows = do
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
  pure $ mkFreeTetromino mapping

freeTetrominos
  :: [FreeTetromino BlockType]
freeTetrominos
  = [ tetromino
    | tetrominoShape <- freeTetrominoShapes
    , Just tetromino <- [parseFreeTetromino tetrominoShape]
    ]


newtype OneSidedTetromino a = OneSidedTetromino
  { unOneSidedTetromino
      :: [FreeTetromino a]  -- infinite rotations
  }
  deriving (Eq, Ord, Show)

mkOneSidedTetromino 
  :: Ord a
  => FreeTetromino a
  -> OneSidedTetromino a
mkOneSidedTetromino
  = OneSidedTetromino
  . fmap mkFreeTetromino
  . iterate (Map.mapKeys (\(V2 x y) -> V2 y (-x)))
  . unFreeTetromino

runOneSidedTetromino
  :: OneSidedTetromino a
  -> FreeTetromino a
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
  for_ rotations $ \tetromino -> do
    printFreeTetromino tetromino
    putStrLn ""

oneSidedTetrominos
  :: [OneSidedTetromino BlockType]
oneSidedTetrominos
  = fmap mkOneSidedTetromino freeTetrominos


test
  :: IO ()
test = do
  for_ oneSidedTetrominos $ \tetromino -> do
    printOneSidedTetromino tetromino
    putStrLn ""
    putStrLn ""
