{-# LANGUAGE CPP #-}
module SDL.Video.Renderer.Shim
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

#ifdef ASTERIUS
import SDL.Video.Renderer.Asterius
#else
import SDL.Video.Renderer
#endif
