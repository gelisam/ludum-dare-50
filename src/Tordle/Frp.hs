{-# LANGUAGE DataKinds, OverloadedLabels, TypeApplications, ImportQualifiedPost, RecursiveDo #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Tordle.Frp where

import Control.Lens
import Control.Monad (guard, when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Char qualified as Char
import Data.Foldable
import Data.Generics.Labels ()
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Traversable
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..), _y)
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
import Tordle.Guess
import Tordle.Model
import Tordle.Rng


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
  let hasKeycode  keycode  keysym = SDL.keysymKeycode keysym == keycode
  let keyUpE    = () <$ filterE (hasKeycode SDL.KeycodeUp) keyPressE
  let keyLeftE  = () <$ filterE (hasKeycode SDL.KeycodeLeft) keyPressE
  let keyDownE  = () <$ filterE (hasKeycode SDL.KeycodeDown) keyPressE
  let keyRightE = () <$ filterE (hasKeycode SDL.KeycodeRight) keyPressE
  let keyEscE   = () <$ filterE (hasKeycode SDL.KeycodeEscape) keyReleaseE

  let keyLetterE
        = givenEvent keyPressE
        $ maybeKeepIt $ \keysym -> do
            let keycode = fromIntegral $ SDL.unwrapKeycode $ SDL.keysymKeycode keysym
            guard (keycode >= Char.ord 'a' && keycode <= Char.ord 'z')
            pure $ Char.toUpper $ Char.chr keycode

  let switchToGuessE
        = landE
      switchToPlaceE
        = () <$ pickLetterE
      switchToEndScreenE
        = gameOverE
       <> winE
  canGuessB <- changingB True
    [ onEvent switchToGuessE     $ setValue $ \() -> True
    , onEvent switchToPlaceE     $ setValue $ \() -> False
    , onEvent switchToEndScreenE $ setValue $ \() -> False
    ]
  canPlaceB <- changingB False
    [ onEvent switchToGuessE     $ setValue $ \() -> False
    , onEvent switchToPlaceE     $ setValue $ \() -> True
    , onEvent switchToEndScreenE $ setValue $ \() -> False
    ]
  let pickLetterE
        = whenE canGuessB keyLetterE
      tryRotateClockwiseE
        = whenE canPlaceB keyUpE
      tryRotateCounterclockwiseE
        = never
      tryMoveLeftE
        = whenE canPlaceB keyLeftE
      tryMoveRightE
        = whenE canPlaceB keyRightE
      userTriesToMoveDownE
        = whenE canPlaceB keyDownE
      gravityE
        = whenE canPlaceB gravityTickE
      tryMoveDownE
        = userTriesToMoveDownE <> gravityE

  timeB <- stepper 0 timeE
  let gravityTickE
        = fmap (const ())
        $ filterE id
        $ (<=) <$> nextGravityTickB <@> timeE
  nextGravityTickB <- changingB 1
    [ onEvent userTriesToMoveDownE $ withBehaviour timeB $ setValue $ \((),t) -> t + 1
    , onEvent gravityTickE         $ withBehaviour timeB $ setValue $ \((),t) -> t + 1
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
        -> Piece
        -> Bool
      isMoveLegal board
        = all (onBlankSpace board)
        . Map.keys
        . renderPiece

      isRowComplete
        :: Board
        -> CInt
        -> Bool
      isRowComplete board y
        = and
            [ V2 x y `Map.member` board
            | x <- xCoordinates
            ]

      isGameOver
        :: Board
        -> Bool
      isGameOver
        = any (not . inMainBoard)
        . Map.keys

  let landE
        = givenEvent tryMoveDownE
        $ withBehaviour boardB
        $ withBehaviour oneSidedTetrominoB
        $ withBehaviour piecePosB
        $ maybeKeepIt $ \((((), board), oneSidedTetromino), piecePos) -> do
            let piecePos' = piecePos + towards S
            let piece' = Piece oneSidedTetromino piecePos'
            guard (not $ isMoveLegal board piece')
  let landingBlocksE
        = givenEvent landE
        $ withBehaviour currentPieceB
        $ transformIt $ \((), piece)
       -> set (each . #blockStatus) InIncompleteWord
        $ renderPiece piece
  let completedRowsE
        :: Event [CInt]
      completedRowsE
        = givenEvent landingBlocksE
        $ withSimultaneousEvent boardWithWildBlocksE
        $ transformIt $ \(landingBlocks, board)
       -> filter (isRowComplete board)
        $ Set.toList
        $ Set.fromList
        $ fmap (view _y)
        $ Map.keys
        $ landingBlocks

  guessesE <- changingRandomlyE undefined
    [ onEvent completedRowsE
    $ withSimultaneousEvent boardWithWildBlocksE
    $ setValueRandomly $ \(completedRows, board) -> do
        for completedRows $ \y -> do
          let getLabel x y = board ^?! ix (V2 x y) . #blockLabel
          let labels = map (flip getLabel y) xCoordinates
          randomCompletion assets labels
    ]
  let coloringsE
        = givenEvent guessesE
        $ transformIt
        $ fmap (analyzeGuess correctWord)

  correctWord <- randomWord assets

  firstOneSidedTetromino <- randomOneSidedTetromino '?'
  oneSidedTetrominoB <- changingRandomlyB firstOneSidedTetromino
    ( [ onEvent landE
      $ setValueRandomly $ \() -> do
          oneSidedTetromino <- randomOneSidedTetromino '?'
          pure oneSidedTetromino
      , onEvent pickLetterE
      $ changeStateOf #theValue $ \letter -> do
          mapped . #_Letter .= letter
      ]
   ++ [ onEvent rotateE
      $ withBehaviour boardB
      $ withBehaviour piecePosB
      $ changeStateOf #theValue $ \(((), board),piecePos) -> do
          oneSidedTetromino <- get
          let oneSidedTetromino' = rotate oneSidedTetromino
          when (isMoveLegal board $ Piece oneSidedTetromino' piecePos) $ do
            put oneSidedTetromino'
      | (rotate, rotateE) <-
          [ (rotateLeft, tryRotateCounterclockwiseE)
          , (rotateRight, tryRotateClockwiseE)
          ]
      ]
    )

  let firstPos = V2 2 1
  piecePosB <- changingB firstPos
    ( [ onEvent landE $ setValue $ \() -> firstPos
      ]
   ++ [ onEvent dirE
      $ withBehaviour boardB
      $ withBehaviour oneSidedTetrominoB
      $ changeState $ \(((), board),oneSidedTetromino) -> do
          piecePos <- get
          let piecePos' = piecePos + towards dir
          when (isMoveLegal board $ Piece oneSidedTetromino piecePos') $ do
            put piecePos'
      | (dir, dirE) <- [(E, tryMoveRightE), (W, tryMoveLeftE), (S, tryMoveDownE)]
      ]
    )

  let boardWithWildBlocksE
        = givenEvent landingBlocksE
        $ withBehaviour boardB
        $ transformIt $ \(landingBlocks, board)
       -> board <> landingBlocks
  let boardWithLetterBlocksE
        = givenEvent completedRowsE
        $ withSimultaneousEvent guessesE
        $ withSimultaneousEvent boardWithWildBlocksE
        $ transformIt $ \((completedRows, guesses), board)
       -> flip execState board $ do
            for_ (zip completedRows guesses) $ \(y, guess) -> do
              for_ (zip [0..] guess) $ \(x, letter) -> do
                ix (V2 x y) . #blockLabel .= Letter letter
  let boardWithColoredBlocksE
        = givenEvent completedRowsE
        $ withSimultaneousEvent coloringsE
        $ withSimultaneousEvent boardWithLetterBlocksE
        $ transformIt $ \((completedRows, colorings), board)
       -> flip execState board $ do
            for_ (zip completedRows colorings) $ \(y, guessResults) -> do
              for_ (zip [0..] guessResults) $ \(x, guessResult) -> do
                ix (V2 x y) . #blockStatus .= guessStatus guessResult
  let boardWithReorderedRowsE
        = givenEvent completedRowsE
        $ withSimultaneousEvent guessesE
        $ withSimultaneousEvent boardWithColoredBlocksE
        $ transformIt $ \((completedRows, guesses), board)
       -> flip performRowActions board
        $ Map.fromList
        $ flip fmap (zip completedRows guesses) $ \(y, guess)
       -> (y, if isRealWord assets guess then MoveRowToBottom else DeleteRow)
  boardB <- changingB Map.empty
    [ onEvent boardWithReorderedRowsE
    $ setValue id
    ]

  let gameOverE
        = givenEvent landingBlocksE
        $ maybeKeepIt $ \landingBlocks -> do
            guard (isGameOver landingBlocks)
  let winE
        = givenEvent coloringsE
        $ maybeKeepIt $ \colorings -> do
            guard $ any (all (== Green)) colorings
  worldStatusB <- changingB Guessing
    [ onEvent gameOverE $ setValue $ \() -> GameOver
    , onEvent winE      $ setValue $ \() -> Win
    ]

  let currentPieceB = Piece <$> oneSidedTetrominoB <*> piecePosB
  let worldB = World <$> worldStatusB <*> boardB <*> currentPieceB
  lift $ reactimate (presentWorld window renderer assets <$> worldB <@ timeE)

  lift $ reactimate ( Mixer.play (assetsMoveSoundEffect assets)
                   <$ mconcat [tryMoveLeftE, tryMoveRightE, userTriesToMoveDownE]
                    )
  lift $ reactimate ( Mixer.play (assetsGameOverSoundEffect assets)
                   <$ gameOverE
                    )

  lift $ reactimate (quit <$ keyEscE)
