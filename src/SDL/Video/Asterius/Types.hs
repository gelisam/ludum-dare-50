module SDL.Video.Asterius.Types
  ( Rectangle(..)
  , Renderer(..)
  , RendererConfig(..)
  , Surface(..)
  , Texture(..)
  , TextureInfo(..)
  , Window(..)
  , WindowConfig(..)
  ) where

import Asterius.Types (JSVal)
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import SDL.Asterius (Point)


data Rectangle a
  = Rectangle (Point V2 a) (V2 a)

data Renderer = Renderer
  { rendererContext
      :: JSVal
  , rendererWindow
      :: Window
  }

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

data Window = Window
  { windowCurrentSize
      :: V2 CInt
  , windowCanvas
      :: JSVal
  }

data WindowConfig = WindowConfig
  { windowInitialSize
      :: V2 CInt
  }
  deriving Show
