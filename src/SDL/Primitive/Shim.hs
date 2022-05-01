module SDL.Primitive.Shim where

import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import SDL.Video.Shim.Types


type Color = V4 Word8
type Pos = V2 CInt

fillRectangle
  :: Renderer
  -> Pos
  -> Pos
  -> Color
  -> IO ()
fillRectangle _ _ _ _ = do
  putStrLn "Primitive.fillRectangle: stub"

rectangle
  :: Renderer
  -> Pos
  -> Pos
  -> Color
  -> IO ()
rectangle _ _ _ _ = do
  putStrLn "Primitive.rectangle: stub"
