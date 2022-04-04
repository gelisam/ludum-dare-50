{-# LANGUAGE ImportQualifiedPost, NamedFieldPuns, RecordWildCards, ScopedTypeVariables #-}
module Tordle.Draw where

import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.Map ((!), Map)
import Data.Map qualified as Map
import Data.StateVar (($=), get)
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..), _x, _y)
import Linear.V4 (V4(..))
import SDL.Primitive (Color, Pos)
import SDL.Primitive qualified as Primitive
import SDL.Video (Window)
import SDL.Video qualified as Video
import SDL.Video.Renderer (Renderer, Texture)
import SDL.Video.Renderer qualified as Renderer
import SDL.Extra
import Tordle.Assets
import Tordle.Colors
import Tordle.Model


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

drawBoard
  :: Renderer
  -> Assets
  -> Board
  -> Pos
  -> IO ()
drawBoard renderer assets board center = do
  let topLeft
        :: Pos
      topLeft
        = center - half ((bOARD_SIZE - 1) * bLOCK_STRIDE)
  for_ [0..bOARD_SIZE^._y - 1] $ \j -> do
    for_ [0..bOARD_SIZE^._x -1] $ \i -> do
      let ij = V2 i j
      drawBlock renderer assets (Map.lookup ij board) (topLeft + ij * bLOCK_STRIDE)

drawWorld
  :: Window
  -> Renderer
  -> Assets
  -> World
  -> IO ()
drawWorld window renderer assets (World {..}) = do
  windowSize <- get $ Video.windowSize window
  Renderer.rendererDrawColor renderer $= V4 255 255 255 255
  Renderer.clear renderer
  drawCenteredTexture renderer (assetsTitleTexture assets) (V2 (windowSize^._x `div` 2) 50)
  drawBoard renderer assets worldBoard (half windowSize)
