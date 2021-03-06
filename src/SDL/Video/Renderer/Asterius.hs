{-# LANGUAGE NamedFieldPuns #-}
module SDL.Video.Renderer.Asterius
  ( Renderer
  , RendererConfig(..)
  , Texture
  , TextureInfo(..)
  , clear
  , copy
  , createTextureFromSurface
  , destroyTexture
  , freeSurface
  , defaultRenderer
  , present
  , queryTexture
  , rendererDrawColor
  ) where

import Data.StateVar
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import SDL.Primitive.Asterius
import SDL.Video.Asterius.Types


clear
  :: Renderer
  -> IO ()
clear renderer = do
  let V2 w h = windowCurrentSize . rendererWindow $ renderer
  js_fillRect (rendererContext renderer)
    0
    0
    (fromIntegral w)
    (fromIntegral h)

copy
  :: Renderer
  -> Texture
  -> Maybe (Rectangle CInt)
  -> Maybe (Rectangle CInt)
  -> IO ()
copy _ _ _ _ = do
  putStrLn "Renderer.copy: stub"

createTextureFromSurface
  :: Renderer
  -> Surface
  -> IO Texture
createTextureFromSurface _ _ = do
  putStrLn "Rendrer.createTextureFromSurface: stub"
  pure TextureStub

destroyTexture
  :: Texture
  -> IO ()
destroyTexture _ = do
  putStrLn "Renderer.destroyTexture: stub"

freeSurface
  :: Surface
  -> IO ()
freeSurface _ = do
  putStrLn "Renderer.freeSurface: stub"

defaultRenderer
  :: RendererConfig
defaultRenderer
  = RendererConfigStub

present
  :: Renderer
  -> IO ()
present _ = do
  putStrLn "Renderer.present: stub"

queryTexture
  :: Texture
  -> IO TextureInfo
queryTexture _ = do
  putStrLn "Renderer.queryTexture: stub"
  pure $ TextureInfo 0 0

rendererDrawColor
  :: Renderer
  -> StateVar Color
rendererDrawColor renderer = StateVar getter setter
  where
    getter
      :: IO Color
    getter = do
      putStrLn "Renderer.windowSize / get: stub"
      pure 0

    setter
      :: Color
      -> IO ()
    setter color = do
      setFillColor renderer color
      setStrokeColor renderer color
