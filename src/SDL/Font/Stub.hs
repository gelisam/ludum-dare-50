module SDL.Font.Stub where

import Data.Text
import SDL.Primitive.Stub (Color)
import SDL.Video.Stub.Types


data Font
  = FontStub
  deriving Show

type PointSize = Int

blended
  :: Font
  -> Color
  -> Text
  -> IO Surface
blended _ _ _ = do
  putStrLn "Font.blended: stub"
  pure SurfaceStub

free
  :: Font
  -> IO ()
free _ = do
  putStrLn "Font.free: stub"

initialize
  :: IO ()
initialize = do
  putStrLn "Font.initialize: stub"

load
  :: FilePath
  -> PointSize
  -> IO Font
load _ _ = do
  putStrLn "Font.load: stub"
  pure FontStub

quit
  :: IO ()
quit = do
  putStrLn "Font.quit: stub"
