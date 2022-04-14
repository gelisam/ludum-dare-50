{-# LANGUAGE DataKinds, OverloadedLabels, TypeApplications, ImportQualifiedPost, RecursiveDo #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Tordle.Frp where

import Control.Lens (filtered)
import Control.Monad (when)
import Control.Monad.Trans.State
import Data.Function.Extra
import Data.Generics.Labels ()
import Data.Map qualified as Map
import Data.Set qualified as Set
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import Reactive.Banana.Combinators
import Reactive.Banana.Extra
import Reactive.Banana.Frameworks
import SDL qualified
import SDL.Mixer qualified as Mixer
import SDL.Video (Window)
import SDL.Video.Renderer (Renderer)
import System.Random (StdGen)
import Tordle.Assets
import Tordle.Dir
import Tordle.Draw
import Tordle.Model
import Tordle.Rng
import Tordle.Tetromino


frpNetwork
  :: Window
  -> Renderer
  -> Assets
  -> StdGen
  -> Event SDL.EventPayload
  -> Event Double  -- ^ time
  -> IO ()  -- ^ quit
  -> MomentIO ()
frpNetwork window renderer assets rng0 sdlE timeE quit = mdo
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
  let cwE    = () <$ filterE (anyP [hasScancode SDL.ScancodeW, hasScancode SDL.ScancodeK, hasKeycode SDL.KeycodeUp, hasScancode SDL.ScancodeE]) keyPressE
  let ccwE   = () <$ filterE (anyP [                                                                                hasScancode SDL.ScancodeQ]) keyPressE
  let leftE  = () <$ filterE (anyP [hasScancode SDL.ScancodeA, hasScancode SDL.ScancodeH, hasKeycode SDL.KeycodeLeft])                          keyPressE
  let downE  = () <$ filterE (anyP [hasScancode SDL.ScancodeS, hasScancode SDL.ScancodeJ, hasKeycode SDL.KeycodeDown])                          keyPressE
  let rightE = () <$ filterE (anyP [hasScancode SDL.ScancodeD, hasScancode SDL.ScancodeL, hasKeycode SDL.KeycodeRight])                         keyPressE
  let escE   = () <$ filterE (hasKeycode SDL.KeycodeEscape) keyReleaseE

  timeB <- stepper 0 timeE
  let gravityE
        = fmap (const ())
        $ filterE id
        $ (<=) <$> nextGravityB <@> timeE
  nextGravityB <- changingB 1
    [ onEvent gravityE $ withBehaviour timeB $ setValue $ \((),t) -> t + 1
    , onEvent downE    $ withBehaviour timeB $ setValue $ \((),t) -> t + 1
    ]

  let fitsInFullBoard
        :: OneSidedTetromino Label
        -> V2 CInt
        -> Bool
      fitsInFullBoard oneSidedTetromino pos
        = all inFullBoard
        $ Map.keys
        $ renderPiece
        $ Piece oneSidedTetromino pos
  let letters = Set.fromList ['A'..'Z']
  let (firstOneSidedTetromino, _rng1) = runState (randomOneSidedTetromino letters) rng0
  pieceBlocksB <- changingB firstOneSidedTetromino
    [ onEvent rotateE
    $ withBehaviour piecePosB
    $ changeState $ \((),piecePos) -> do
        oneSidedTetromino <- get
        let oneSidedTetromino' = rotate oneSidedTetromino
        when (fitsInFullBoard oneSidedTetromino' piecePos) $ do
          put oneSidedTetromino'
    | (rotate, rotateE) <- [(rotateLeft, ccwE), (rotateRight, cwE)]
    ]
  piecePosB <- changingB (V2 2 1)
    [ onEvent dirE
    $ withBehaviour pieceBlocksB
    $ changeState $ \((),pieceBlocks) -> do
        piecePos <- get
        let piecePos' = piecePos + towards dir
        when (fitsInFullBoard pieceBlocks piecePos') $ do
          put piecePos'
    | (dir, dirE) <- [(E, rightE), (W, leftE), (S, downE), (S, gravityE)]
    ]
  let boardB = pure Map.empty
  let currentPieceB = Piece <$> pieceBlocksB <*> piecePosB
  let worldB = World <$> boardB <*> currentPieceB
  reactimate (presentWorld window renderer assets <$> worldB <@ timeE)
  reactimate (Mixer.play (assetsMoveSoundEffect assets) <$ mconcat [leftE, downE, rightE])
  reactimate (quit <$ escE)
