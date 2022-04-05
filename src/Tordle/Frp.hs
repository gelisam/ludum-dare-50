{-# LANGUAGE DataKinds, OverloadedLabels, TypeApplications, ImportQualifiedPost, RecursiveDo #-}
module Tordle.Frp where

import Control.Lens (filtered)
import Data.Function.Extra
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Map qualified as Map
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import Reactive.Banana.Combinators
import Reactive.Banana.Extra
import Reactive.Banana.Frameworks
import SDL qualified
import SDL.Mixer qualified as Mixer
import SDL.Video (Window)
import SDL.Video.Renderer (Renderer)
import Tordle.Assets
import Tordle.Dir
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
frpNetwork window renderer assets sdlE timeE quit = mdo
  let keyboardE = filterPrismE #_KeyboardEvent sdlE
  let keyPressE = filterPrismE ( filtered (\e -> SDL.keyboardEventKeyMotion e == SDL.Pressed)
                               . #keyboardEventKeysym
                               )
                               keyboardE
  let keyReleaseE = filterPrismE ( filtered (\e -> SDL.keyboardEventKeyMotion e == SDL.Released)
                                 . #keyboardEventKeysym
                                 )
                                 keyboardE
  let hasScancode scancode keysym = SDL.keysymScancode keysym == scancode
  let hasKeycode  keycode  keysym = SDL.keysymKeycode  keysym == keycode

  -- WASD, vim-style HJKL, or simply arrow keys
  let upE    = () <$ filterE (anyP [hasScancode SDL.ScancodeW, hasScancode SDL.ScancodeK, hasKeycode SDL.KeycodeUp])      keyPressE
  let leftE  = () <$ filterE (anyP [hasScancode SDL.ScancodeA, hasScancode SDL.ScancodeH, hasKeycode SDL.KeycodeLeft])    keyPressE
  let downE  = () <$ filterE (anyP [hasScancode SDL.ScancodeS, hasScancode SDL.ScancodeJ, hasKeycode SDL.KeycodeDown])    keyPressE
  let rightE = () <$ filterE (anyP [hasScancode SDL.ScancodeD, hasScancode SDL.ScancodeL, hasKeycode SDL.KeycodeRight])   keyPressE
  let escE   = () <$ filterE (hasKeycode SDL.KeycodeEscape) keyReleaseE

  let pieceBlocksB = pure $ Map.fromList
        [ (V2 1 0, Letter 'A')
        , (V2 1 1, Wild)
        , (V2 2 1, Wild)
        , (V2 2 2, Letter 'B')
        ]
  let fitsInFullBoard
        :: Map (V2 CInt) Label
        -> V2 CInt
        -> Bool
      fitsInFullBoard blocks pos
        = all inFullBoard
        $ Map.keys
        $ renderPiece
        $ Piece blocks pos
  piecePosB <- steppersB (V2 1 1)
    [ filterBE (fitsInFullBoard <$> pieceBlocksB)
    $ (+) <$> piecePosB <@> (towards dir <$ dirE)
    | (dir, dirE) <- [(N, upE), (E, rightE), (W, leftE), (S, downE)]
    ]
  let boardB = pure Map.empty
  let currentPieceB = Piece <$> pieceBlocksB <*> piecePosB
  let worldB = World <$> boardB <*> currentPieceB
  reactimate (presentWorld window renderer assets <$> worldB <@ timeE)
  reactimate (Mixer.play (assetsMoveSoundEffect assets) <$ mconcat [upE, leftE, downE, rightE])
  reactimate (quit <$ escE)
