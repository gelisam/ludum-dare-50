{-# LANGUAGE ImportQualifiedPost, NamedFieldPuns, NumericUnderscores, OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}
module Tordle (main) where

import Control.Concurrent (threadDelay)
import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.Map ((!), Map)
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
import SDL.Video.Renderer (Renderer, Texture)
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
      (pos + half bLOCK_SIZE - fromIntegral i + 1)
      color

drawLetter
  :: Renderer
  -> Map Char Texture
  -> Char
  -> Pos
  -> IO ()
drawLetter renderer letterTextures char pos = do
  drawCenteredTexture renderer (letterTextures ! char) pos

drawLabel
  :: Renderer
  -> Map Char Texture
  -> Label
  -> Pos
  -> IO ()
drawLabel renderer letterTextures label pos = do
  case label of
    Letter char -> do
      drawLetter renderer letterTextures char pos
    Wild -> do
      pure ()


drawBlock
  :: Renderer
  -> Assets
  -> Maybe Block
  -> Pos
  -> IO ()
drawBlock renderer (Assets {assetsBlackLetterTextures, assetsWhiteLetterTextures}) maybeBlock pos = do
  case maybeBlock of
    Nothing -> do
      drawOutlineBlock renderer pos lightGray
    Just (Block {..}) -> do
      case blockStatus of
        Falling -> do
          drawSolidBlock renderer pos blue
          drawLabel renderer assetsWhiteLetterTextures blockLabel pos
        InIncompleteWord -> do
          drawOutlineBlock renderer pos gray
          drawLabel renderer assetsBlackLetterTextures blockLabel pos
        NotInWord -> do
          drawSolidBlock renderer pos gray
          drawLabel renderer assetsWhiteLetterTextures blockLabel pos
        WrongSpot -> do
          drawSolidBlock renderer pos yellow
          drawLabel renderer assetsWhiteLetterTextures blockLabel pos
        CorrectSpot -> do
          drawSolidBlock renderer pos green
          drawLabel renderer assetsWhiteLetterTextures blockLabel pos


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
            drawBlock renderer assets Nothing                                      (half windowSize - 3 * unit _x * bLOCK_STRIDE)
            drawBlock renderer assets (Just $ Block Wild Falling)                  (half windowSize - 2 * unit _x * bLOCK_STRIDE)
            drawBlock renderer assets (Just $ Block (Letter 'O') Falling)          (half windowSize - 1 * unit _x * bLOCK_STRIDE)
            drawBlock renderer assets (Just $ Block (Letter 'R') InIncompleteWord) (half windowSize + 0 * unit _x * bLOCK_STRIDE)
            drawBlock renderer assets (Just $ Block (Letter 'D') NotInWord)        (half windowSize + 1 * unit _x * bLOCK_STRIDE)
            drawBlock renderer assets (Just $ Block (Letter 'L') WrongSpot)        (half windowSize + 2 * unit _x * bLOCK_STRIDE)
            drawBlock renderer assets (Just $ Block (Letter 'E') CorrectSpot)      (half windowSize + 3 * unit _x * bLOCK_STRIDE)
            Renderer.present renderer
            Mixer.play (assetsMoveSoundEffect assets)
            threadDelay 3_000_000
