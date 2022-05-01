{-# LANGUAGE OverloadedStrings #-}
module Tordle (main) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Data.Function (fix)
import Data.IORef
import Foreign.C.Types (CInt)
import Linear (V2(..))
import Loop.Shim (scheduleNextFrame)
import qualified Reactive.Banana.Frameworks as Banana
import qualified SDL.Shim as SDL
import qualified SDL.Mixer.Shim as Mixer
import qualified SDL.Video.Shim as Video
import qualified SDL.Video.Renderer.Shim as Renderer
import SDL.Extra
import System.Random.Shim
import Tordle.Assets
import Tordle.Frp


main
  :: IO ()
main = do
  SDL.initializeAll
  withTTF $ do
    Mixer.withAudio Mixer.defaultAudio 1024 $ do
      let windowConfig
            :: Video.WindowConfig
          windowConfig
            = Video.defaultWindow
            { Video.windowInitialSize
                = V2 800 800
            }
      withWindow "Tordle" windowConfig $ \window -> do
        withRenderer window 0 Renderer.defaultRenderer $ \renderer -> do
          withAssets renderer $ \assets -> do
            time0 <- SDL.time
            (sdlPayloadAddHandler, fireSdlPayload) <- Banana.newAddHandler
            (timeAddHandler, fireTime) <- Banana.newAddHandler
            shouldQuitRef <- newIORef False
            let quit
                  :: IO ()
                quit = do
                  writeIORef shouldQuitRef True
            eventNetwork <- Banana.compile $ do
              sdlE <- Banana.fromAddHandler sdlPayloadAddHandler
              timeE <- Banana.fromAddHandler timeAddHandler
              rng <- liftIO initStdGen
              flip evalStateT rng $ do
                frpNetwork window renderer assets sdlE timeE quit
            Banana.actuate eventNetwork
            fix $ \loop -> do
              shouldQuit <- readIORef shouldQuitRef
              unless shouldQuit $ do
                let fps
                      :: CInt
                    fps
                      = 60
                    ms_per_frame
                      :: CInt
                    ms_per_frame
                      = 1000 `div` fps
                maybeSdlEvent <- SDL.waitEventTimeout ms_per_frame
                let stop
                      :: IO ()
                    stop = do
                      pure ()
                    continue
                      :: IO ()
                    continue = do
                      time <- SDL.time
                      fireTime (time - time0)
                      scheduleNextFrame loop
                case SDL.eventPayload <$> maybeSdlEvent of
                  Just SDL.QuitEvent -> do
                    stop
                  Nothing -> do
                    continue
                  Just sdlPayload -> do
                    fireSdlPayload sdlPayload
                    continue
