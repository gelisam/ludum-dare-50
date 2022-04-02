{-# LANGUAGE ImportQualifiedPost, NamedFieldPuns, NumericUnderscores, OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}
module Tordle (main) where

import Control.Concurrent (threadDelay)
import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.Map ((!))
import Data.StateVar (($=), get)
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..), _x)
import Linear.V4 (V4(..))
import Linear.Vector (unit)
import SDL qualified
import SDL.Mixer qualified as Mixer
import SDL.Primitive (Color, Pos)
import SDL.Primitive qualified as Primitive
import SDL.Video qualified as Video
import SDL.Video.Renderer (Renderer)
import SDL.Video.Renderer qualified as Renderer
import SDL.Extra
import Tordle.Assets
import Tordle.Colors


data BlockStatus
  = Falling
  | InIncompleteWord
  | NotInWord
  | WrongSpot
  | CorrectSpot
  deriving Show

data Block = Block
  { blockLetter
      :: Char
  , blockStatus
      :: BlockStatus
  }


bLOCK_SIZE
  :: Num a => a
bLOCK_SIZE
  = 40

bLOCK_GAP
  :: Num a => a
bLOCK_GAP
  = 4

bLOCK_STRIDE
  :: Num a => a
bLOCK_STRIDE
  = bLOCK_SIZE + bLOCK_GAP

oUTLINE_SIZE
  :: Num a => a
oUTLINE_SIZE
  = 2


drawSolidBlock
  :: Renderer
  -> Pos  -- center
  -> Color
  -> IO ()
drawSolidBlock renderer pos color = do
  Primitive.fillRectangle renderer (pos - half bLOCK_SIZE) (pos + half bLOCK_SIZE) color

drawOutlineBlock
  :: Renderer
  -> Pos  -- center
  -> Color
  -> IO ()
drawOutlineBlock renderer pos color = do
  for_ [0..oUTLINE_SIZE-1] $ \(i :: CInt) -> do
    Primitive.rectangle
      renderer
      (pos - half bLOCK_SIZE + fromIntegral i)
      (pos + half bLOCK_SIZE - fromIntegral i)
      color

drawLetter
  :: Renderer
  -> Assets
  -> Char
  -> Pos
  -> Color
  -> IO ()
drawLetter renderer (Assets {assetsLetterTextures}) char pos _color = do
  -- TODO: how to re-color a texture?
  drawCenteredTexture renderer (assetsLetterTextures ! char) pos


drawBlock
  :: Renderer
  -> Assets
  -> Block
  -> Pos
  -> IO ()
drawBlock renderer assets (Block {..}) pos = do
  case blockStatus of
    Falling -> do
      drawSolidBlock renderer pos blue
      drawLetter renderer assets blockLetter pos white
    InIncompleteWord -> do
      drawOutlineBlock renderer pos gray
      drawLetter renderer assets blockLetter pos black
    NotInWord -> do
      drawSolidBlock renderer pos gray
      drawLetter renderer assets blockLetter pos white
    WrongSpot -> do
      drawSolidBlock renderer pos yellow
      drawLetter renderer assets blockLetter pos white
    CorrectSpot -> do
      drawSolidBlock renderer pos green
      drawLetter renderer assets blockLetter pos white


main
  :: IO ()
main = do
  SDL.initializeAll
  withTTF $ do
    Mixer.withAudio Mixer.defaultAudio 1024 $ do
      withWindow "Tordle" Video.defaultWindow $ \window -> do
        windowSize <- get $ Video.windowSize window
        withRenderer window 0 Renderer.defaultRenderer $ \renderer -> do
          withAssets renderer $ \assets -> do
            Renderer.rendererDrawColor renderer $= V4 255 255 255 255
            Renderer.clear renderer
            drawCenteredTexture renderer (assetsTitleTexture assets) (V2 (windowSize^._x `div` 2) 50)
            drawBlock renderer assets (Block 'T' Falling)          (half windowSize - 2 * unit _x * bLOCK_STRIDE)
            drawBlock renderer assets (Block 'O' InIncompleteWord) (half windowSize - 1 * unit _x * bLOCK_STRIDE)
            drawBlock renderer assets (Block 'R' NotInWord)        (half windowSize + 0 * unit _x * bLOCK_STRIDE)
            drawBlock renderer assets (Block 'D' WrongSpot)        (half windowSize + 1 * unit _x * bLOCK_STRIDE)
            drawBlock renderer assets (Block 'L' CorrectSpot)      (half windowSize + 2 * unit _x * bLOCK_STRIDE)
            Renderer.present renderer
            Mixer.play (assetsMoveSoundEffect assets)
            threadDelay 1_000_000
