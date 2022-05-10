{-# LANGUAGE NamedFieldPuns #-}
module SDL.Primitive.Asterius where

import Asterius.Types
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import SDL.Video.Asterius.Types


foreign import javascript
  "(() => {                   \
  \  $1.beginPath();          \
  \  $1.fillRect($2, $3, $4, $5); \
  \  $1.stroke();             \
  \})()"
  js_fillRect
    :: JSVal
    -> Int  -- ^ top-left x
    -> Int  -- ^ top-left y
    -> Int  -- ^ width
    -> Int  -- ^ height
    -> IO ()

foreign import javascript
  "(() => {                                                                \
  \  $1.fillStyle = 'rgba(' + $2 + ',' + $3 + ',' + $4 + ',' + $5 + ')'; \
  \})()"
  js_setFillColor
    :: JSVal
    -> Word8  -- ^ red, 0-255
    -> Word8  -- ^ green, 0-255
    -> Word8  -- ^ blue, 0-255
    -> Word8  -- ^ alpha, 0-255
    -> IO ()

foreign import javascript
  "(() => {                                                                \
  \  $1.strokeStyle = 'rgba(' + $2 + ',' + $3 + ',' + $4 + ',' + $5 + ')'; \
  \})()"
  js_setStrokeColor
    :: JSVal
    -> Word8  -- ^ red, 0-255
    -> Word8  -- ^ green, 0-255
    -> Word8  -- ^ blue, 0-255
    -> Word8  -- ^ alpha, 0-255
    -> IO ()

foreign import javascript
  "(() => {                   \
  \  $1.beginPath();          \
  \  $1.rect($2, $3, $4, $5); \
  \  $1.stroke();             \
  \})()"
  js_strokeRect
    :: JSVal
    -> Int  -- ^ top-left x
    -> Int  -- ^ top-left y
    -> Int  -- ^ width
    -> Int  -- ^ height
    -> IO ()


type Color = V4 Word8
type Pos = V2 CInt

fillRectangle
  :: Renderer
  -> Pos
  -> Pos
  -> Color
  -> IO ()
fillRectangle renderer (V2 x1 y1) (V2 x2 y2) color = do
  let w = x2 - x1
  let h = y2 - y1
  setFillColor renderer color
  js_fillRect (rendererContext renderer)
    (fromIntegral x1)
    (fromIntegral y1)
    (fromIntegral w)
    (fromIntegral h)

rectangle
  :: Renderer
  -> Pos
  -> Pos
  -> Color
  -> IO ()
rectangle renderer (V2 x1 y1) (V2 x2 y2) color = do
  let w = x2 - x1
  let h = y2 - y1
  setFillColor renderer color
  js_fillRect (rendererContext renderer)
    (fromIntegral x1)
    (fromIntegral y1)
    (fromIntegral w)
    (fromIntegral 1)
  js_fillRect (rendererContext renderer)
    (fromIntegral x1)
    (fromIntegral y1)
    (fromIntegral 1)
    (fromIntegral h)
  js_fillRect (rendererContext renderer)
    (fromIntegral x1)
    (fromIntegral (y2-1))
    (fromIntegral w)
    (fromIntegral 1)
  js_fillRect (rendererContext renderer)
    (fromIntegral (x2-1))
    (fromIntegral y1)
    (fromIntegral 1)
    (fromIntegral h)

setFillColor
  :: Renderer
  -> Color
  -> IO ()
setFillColor (Renderer {rendererContext}) (V4 r g b a) = do
  js_setFillColor rendererContext r g b a

setStrokeColor
  :: Renderer
  -> Color
  -> IO ()
setStrokeColor (Renderer {rendererContext}) (V4 r g b a) = do
  js_setStrokeColor rendererContext r g b a
