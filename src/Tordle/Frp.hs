{-# LANGUAGE DataKinds, OverloadedLabels, TypeApplications, LambdaCase, RecursiveDo #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Tordle.Frp where

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Generics.Labels ()
import qualified Data.Map as Map
import Linear.V2 (V2(..))
import Reactive.Banana.Combinators
import Reactive.Banana.Extra
import Reactive.Banana.Frameworks
import qualified SDL.Shim as SDL
import SDL.Video.Shim (Window)
import SDL.Video.Renderer.Shim (Renderer)
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

  let hasKeycode keycode keysym
        = SDL.keysymKeycode keysym == keycode
  let keyLeftE  = () <$ filterE (hasKeycode SDL.KeycodeLeft) keyPressE
  let keyDownE  = () <$ filterE (hasKeycode SDL.KeycodeDown) keyPressE
  let keyRightE = () <$ filterE (hasKeycode SDL.KeycodeRight) keyPressE
  let keyEscE   = () <$ filterE (hasKeycode SDL.KeycodeEscape) keyPressE

  let tryMoveLeftE
        = keyLeftE
      tryMoveRightE
        = keyRightE
      userTriesToMoveDownE
        = keyDownE
      tryMoveDownE
        = userTriesToMoveDownE

  let mkQuestionMarkTetromino
        :: FixedTetromino BlockType
        -> OneSidedTetromino Label
      mkQuestionMarkTetromino
        = fmap addQuestionMark
        . mkOneSidedTetromino
        where
          addQuestionMark
            :: BlockType
            -> Label
          addQuestionMark Unlabelled
            = Wild
          addQuestionMark Labelled
            = Letter '?'
  firstFixedTetrominoIndex <- randomIndex fixedTetrominos
  let firstFixedTetromino
        = fixedTetrominos !! firstFixedTetrominoIndex
  let firstOneSidedTetromino = mkQuestionMarkTetromino firstFixedTetromino
  let firstPos = V2 2 1
  piecePosB <- changingB firstPos
    ( [ onEvent dirE
      $ changeState $ \() -> do
          piecePos <- get
          let piecePos' = piecePos + towards dir
          put piecePos'
      | (dir, dirE) <- [(E, tryMoveRightE), (W, tryMoveLeftE), (S, tryMoveDownE)]
      ]
    )

  let currentPieceB
          = Piece
        <$> pure firstOneSidedTetromino
        <*> piecePosB
  let worldB
          = World
        <$> pure Playing
        <*> pure Nothing
        <*> pure Nothing
        <*> pure Map.empty
        <*> pure Map.empty
        <*> currentPieceB
  lift $ reactimate (presentWorld window renderer assets <$> worldB <@ timeE)

  lift $ reactimate (quit <$ keyEscE)
