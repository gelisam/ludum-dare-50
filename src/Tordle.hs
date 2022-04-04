{-# LANGUAGE ImportQualifiedPost, NamedFieldPuns, NumericUnderscores, OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}
module Tordle (main) where

import Control.Concurrent (threadDelay)
import Data.Map qualified as Map
import Linear.V2 (V2(..))
import SDL qualified
import SDL.Mixer qualified as Mixer
import SDL.Video qualified as Video
import SDL.Video.Renderer qualified as Renderer
import SDL.Extra
import Tordle.Assets
import Tordle.Draw
import Tordle.Model


main
  :: IO ()
main = do
  SDL.initializeAll
  withTTF $ do
    Mixer.withAudio Mixer.defaultAudio 1024 $ do
      withWindow "Tordle" Video.defaultWindow $ \window -> do
        withRenderer window 0 Renderer.defaultRenderer $ \renderer -> do
          withAssets renderer $ \assets -> do
            drawWorld window renderer assets $ World $ Map.fromList
              [ (V2 1 1, Block (Letter 'A') Falling)
              , (V2 1 2, Block Wild Falling)
              , (V2 2 2, Block Wild Falling)
              , (V2 2 3, Block (Letter 'B') Falling)
              , (V2 0 5, Block (Letter 'C') InIncompleteWord)
              , (V2 1 5, Block Wild InIncompleteWord)
              , (V2 4 5, Block (Letter 'D') InIncompleteWord)
              , (V2 5 5, Block (Letter 'E') InIncompleteWord)
              ]
            Renderer.present renderer
            Mixer.play (assetsMoveSoundEffect assets)
            threadDelay 5_000_000
