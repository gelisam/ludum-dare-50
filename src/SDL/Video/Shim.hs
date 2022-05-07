{-# LANGUAGE CPP #-}
module SDL.Video.Shim
  ( Rectangle(..)
  , Surface
  , Texture
  , Window
  , WindowConfig(..)
  , createWindow
  , destroyWindow
  , createRenderer
  , defaultWindow
  , destroyRenderer
  , windowSize
  ) where

#ifdef ASTERIUS

import Data.StateVar
import Data.Text (Text)
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import SDL.Video.Shim.Types


createWindow
  :: Text  -- title
  -> WindowConfig
  -> IO Window
createWindow title windowConfig = do
  putStrLn "Video.createWindow: stub"
  pure WindowStub

destroyWindow
  :: Window
  -> IO ()
destroyWindow _ = do
  putStrLn "Video.destroyWindow: stub"

createRenderer
  :: Window
  -> CInt
  -> RendererConfig
  -> IO Renderer
createRenderer _ _ _ = do
  putStrLn "Video.createRenderer: stub"
  pure RendererStub

defaultWindow
  :: WindowConfig
defaultWindow
  = WindowConfig (V2 800 600)

destroyRenderer
  :: Renderer
  -> IO ()
destroyRenderer _ = do
  putStrLn "Video.destroyRenderer: stub"

windowSize
  :: Window
  -> StateVar (V2 CInt)
windowSize _ = StateVar getter setter
  where
    getter
      :: IO (V2 CInt)
    getter = do
      putStrLn "Video.windowSize / get: stub"
      pure 0

    setter
      :: V2 CInt
      -> IO ()
    setter _ = do
      putStrLn "Video.windowSize / set: stub"


#else
import SDL.Video
#endif
