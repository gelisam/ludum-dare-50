{-# LANGUAGE NamedFieldPuns #-}
module SDL.Video.Asterius
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

import Asterius.Types
import Data.StateVar
import Data.Text (Text)
import qualified Data.Text as Text
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import SDL.Video.Asterius.Types


foreign import javascript
  "(() => {                      \
  \  window.document.title = $1; \
  \})()"
  js_setTitle :: JSString -> IO ()

foreign import javascript
  "(() => {                                           \
  \  const div = document.createElement('div');       \
  \  div.style.width = '' + $1 + 'px';                \
  \  div.style.height = '' + $2 + 'px';               \
  \  div.style.margin = 'auto';                       \
  \  const canvas = document.createElement('canvas'); \
  \  canvas.id = 'canvas';                            \
  \  canvas.width = $1;                               \
  \  canvas.height = $2;                              \
  \  div.appendChild(canvas);                         \
  \  document.body.appendChild(div);                  \
  \  return canvas;                                   \
  \})()"
  js_createCanvas :: Int -> Int -> IO JSVal

foreign import javascript
  "$1.getContext('2d')"
  js_canvasContext :: JSVal -> JSVal


createWindow
  :: Text  -- title
  -> WindowConfig
  -> IO Window
createWindow title windowConfig = do
  js_setTitle $ toJSString $ Text.unpack title
  let V2 w h = windowInitialSize windowConfig
  canvas <- js_createCanvas (fromIntegral w) (fromIntegral h)
  pure $ Window
    { windowCurrentSize
        = windowInitialSize windowConfig
    , windowCanvas
        = canvas
    }

destroyWindow
  :: Window
  -> IO ()
destroyWindow _ = do
  -- intentionally not removing the canvas because the top-level 'bracket' ends
  -- too soon.
  pure ()

createRenderer
  :: Window
  -> CInt
  -> RendererConfig
  -> IO Renderer
createRenderer window@(Window {windowCanvas}) _ _ = do
  pure $ Renderer
    { rendererContext
        = js_canvasContext windowCanvas
    , rendererWindow
        = window
    }

defaultWindow
  :: WindowConfig
defaultWindow
  = WindowConfig (V2 800 600)

destroyRenderer
  :: Renderer
  -> IO ()
destroyRenderer _ = do
  -- nothing to do; the canvas context will disappear with the canvas (or would
  -- if we were removing the canvas).
  pure ()

windowSize
  :: Window
  -> StateVar (V2 CInt)
windowSize (Window {windowCurrentSize}) = StateVar getter setter
  where
    getter
      :: IO (V2 CInt)
    getter = do
      pure windowCurrentSize

    setter
      :: V2 CInt
      -> IO ()
    setter _ = do
      -- wouldn't be too hard to implement, but I don't need it in this game.
      error "Video.windowSize / set: not supported"
