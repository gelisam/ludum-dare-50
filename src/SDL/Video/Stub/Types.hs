module SDL.Video.Stub.Types where

import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import SDL.Stub (Point)


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
