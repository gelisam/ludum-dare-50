{-# LANGUAGE RecordWildCards #-}
module Tordle.Draw where

import Data.StateVar (($=), get)
import Linear.Extra
import Linear.V4 (V4(..))
import qualified SDL.Primitive.Shim as Primitive
import SDL.Video.Renderer.Shim (Renderer)
import SDL.Video.Shim (Window)
import qualified SDL.Video.Shim as Video
import qualified SDL.Video.Renderer.Shim as Renderer
import Tordle.Assets
import Tordle.Model


presentWorld
  :: Window
  -> Renderer
  -> Assets
  -> World
  -> IO ()
presentWorld window renderer _assets (World {..}) = do
  windowSize <- get $ Video.windowSize window
  Renderer.rendererDrawColor renderer $= V4 255 255 255 255
  Renderer.clear renderer
  Primitive.rectangle
    renderer
    (half windowSize - 50)
    (half windowSize)
    (V4 255 0 0 255)
  Primitive.rectangle
    renderer
    (half windowSize)
    (half windowSize + 50)
    (V4 0 0 255 255)
  Renderer.present renderer
