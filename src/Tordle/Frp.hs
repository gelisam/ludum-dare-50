{-# LANGUAGE ImportQualifiedPost #-}
module Tordle.Frp where

import Data.Map qualified as Map
import Linear.V2 (V2(..))
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import SDL qualified
import SDL.Mixer qualified as Mixer
import SDL.Video (Window)
import SDL.Video.Renderer (Renderer)
import Tordle.Assets
import Tordle.Draw
import Tordle.Model


frpNetwork
  :: Window
  -> Renderer
  -> Assets
  -> Event SDL.EventPayload
  -> Event Double  -- ^ time
  -> IO ()  -- ^ quit
  -> MomentIO ()
frpNetwork window renderer assets sdlE timeE quit = do
  let mouseClickE
        :: Event SDL.MouseButtonEventData
      mouseClickE
        = filterJust
        $ fmap (\e -> do SDL.MouseButtonEvent x <- pure e
                         pure x)
        $ sdlE
  let keyboardE
        :: Event SDL.KeyboardEventData
      keyboardE
        = filterJust
        $ fmap (\e -> do SDL.KeyboardEvent x <- pure e
                         pure x)
        $ sdlE

  let worldB
        :: Behavior World
      worldB
        = pure
        $ World $ Map.fromList
            [ (V2 1 0, Block (Letter 'A') Falling)
            , (V2 1 1, Block Wild Falling)
            , (V2 2 1, Block Wild Falling)
            , (V2 2 2, Block (Letter 'B') Falling)
            , (V2 0 9, Block (Letter 'C') InIncompleteWord)
            , (V2 1 9, Block Wild InIncompleteWord)
            , (V2 4 9, Block (Letter 'D') InIncompleteWord)
            , (V2 5 9, Block (Letter 'E') InIncompleteWord)
            ]
  reactimate (presentWorld window renderer assets <$> worldB <@ timeE)
  reactimate (Mixer.play (assetsMoveSoundEffect assets) <$ keyboardE)
  reactimate (quit <$ mouseClickE)
