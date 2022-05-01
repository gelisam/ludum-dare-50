module Tordle.Assets where

import SDL.Video.Renderer.Shim (Renderer)


data Assets = AssetsStub

withAssets
  :: Renderer
  -> (Assets -> IO a)
  -> IO a
withAssets _renderer body = do
  body AssetsStub
