{-# LANGUAGE DataKinds, OverloadedLabels, TypeApplications, ImportQualifiedPost, RecursiveDo #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Tordle.Frp where

import Control.Lens
import Control.Monad (guard, when)
import Control.Monad.Trans.Class
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
import System.Random.Stateful (StdGen)
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
  -> Event SDL.EventPayload
  -> Event Double  -- ^ time
  -> IO ()  -- ^ quit
  -> StateT StdGen MomentIO ()
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

  let onBlankSpace
        :: Board
        -> V2 CInt
        -> Bool
      onBlankSpace board pos
        = inFullBoard pos
       && Map.lookup pos board == Nothing

      isMoveLegal
        :: Board
        -> OneSidedTetromino Label
        -> V2 CInt
        -> Bool
      isMoveLegal board oneSidedTetromino pos
        = all (onBlankSpace board)
        $ Map.keys
        $ renderPiece
        $ Piece oneSidedTetromino pos
  let letters = Set.fromList ['A'..'Z']
  firstOneSidedTetromino <- randomOneSidedTetromino letters
  let firstPos = V2 2 1
  oneSidedTetrominoB <- changingRandomlyB firstOneSidedTetromino
    ( [ onEvent landE $ setValueRandomly $ \() -> do
          oneSidedTetromino <- randomOneSidedTetromino letters
          pure oneSidedTetromino
      ]
   ++ [ onEvent rotateE
      $ withBehaviour boardB
      $ withBehaviour piecePosB
      $ changeStateOf #theValue $ \(((), board),piecePos) -> do
          oneSidedTetromino <- get
          let oneSidedTetromino' = rotate oneSidedTetromino
          when (isMoveLegal board oneSidedTetromino' piecePos) $ do
            put oneSidedTetromino'
      | (rotate, rotateE) <- [(rotateLeft, ccwE), (rotateRight, cwE)]
      ]
    )
  piecePosB <- changingB firstPos
    ( [ onEvent landE $ setValue $ \() -> firstPos
      ]
   ++ [ onEvent dirE
      $ withBehaviour boardB
      $ withBehaviour oneSidedTetrominoB
      $ changeState $ \(((), board),oneSidedTetromino) -> do
          piecePos <- get
          let piecePos' = piecePos + towards dir
          when (isMoveLegal board oneSidedTetromino piecePos') $ do
            put piecePos'
      | (dir, dirE) <- [(E, rightE), (W, leftE), (S, downE), (S, gravityE)]
      ]
    )
  let landE
        = givenEvent (gravityE <> downE)
        $ withBehaviour boardB
        $ withBehaviour oneSidedTetrominoB
        $ withBehaviour piecePosB
        $ maybeKeepIt $ \((((), board), oneSidedTetromino), piecePos) -> do
            let piecePos' = piecePos + towards S
            guard (not $ isMoveLegal board oneSidedTetromino piecePos')
  boardB <- changingB Map.empty
    [ onEvent landE
    $ withBehaviour currentPieceB
    $ changeState $ \((), currentPiece) -> do
        let newBlocks = renderPiece currentPiece
        modify (<> newBlocks)
    ]
  let currentPieceB = Piece <$> oneSidedTetrominoB <*> piecePosB
  let worldB = World <$> boardB <*> currentPieceB
  lift $ reactimate (presentWorld window renderer assets <$> worldB <@ timeE)
  lift $ reactimate (Mixer.play (assetsMoveSoundEffect assets) <$ mconcat [leftE, downE, rightE])
  lift $ reactimate (quit <$ escE)
