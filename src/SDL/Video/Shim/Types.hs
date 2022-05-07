{-# LANGUAGE CPP #-}
module SDL.Video.Shim.Types
  ( Rectangle(..)
#ifdef ASTERIUS
  , Renderer(..)
#else
  , Renderer
#endif
  , RendererConfig(..)
  , Surface(..)
#ifdef ASTERIUS
  , Texture(..)
#else
  , Texture
#endif
  , TextureInfo(..)
#ifdef ASTERIUS
  , Window(..)
#else
  , Window
#endif
  , WindowConfig(..)
  ) where

#ifdef ASTERIUS

import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import SDL.Shim (Point)


data Rectangle a
  = Rectangle (Point V2 a) (V2 a)

data Renderer
  = RendererStub
  deriving Show

data RendererConfig
  = RendererConfigStub
  deriving Show

data Surface
  = SurfaceStub
  deriving Show

data Texture
  = TextureStub
  deriving Show

data TextureInfo = TextureInfo
  { textureWidth
      :: CInt
  , textureHeight
      :: CInt
  }
  deriving Show

data Window
  = WindowStub
  deriving Show

data WindowConfig = WindowConfig
  { windowInitialSize
      :: V2 CInt
  }
  deriving Show


#else
import SDL.Video
#endif
