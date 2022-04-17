{-# LANGUAGE DataKinds, OverloadedLabels, TypeApplications, ImportQualifiedPost, RecursiveDo #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Tordle.Frp where

import Control.Lens
import Control.Monad (guard, when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Foldable
import Data.Function.Extra
import Data.Generics.Labels ()
import Data.List.Extra (mconcatMap)
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
        = givenEvent (gravityE <> downE)
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

  let eliminatedLettersE
        = givenEvent guessesE
        $ withSimultaneousEvent coloringsE
        $ transformIt $ \(guesses, colorings)
       -> flip mconcatMap (zip guesses colorings) $ \(guess, coloring)
       -> flip mconcatMap (zip guess coloring) $ \(letter, guessResult)
       -> Set.fromList
            [ letter
            | guessResult == Grey
            ]
  remainingLettersB <- changingB (Set.fromList allLetters)
    [ onEvent eliminatedLettersE
    $ changeValue $ \(eliminatedLetters, letters)
   -> letters Set.\\ eliminatedLetters
    ]
  correctWord <- randomWord assets

  firstOneSidedTetromino <- randomOneSidedTetromino (Set.fromList allLetters)
  oneSidedTetrominoB <- changingRandomlyB firstOneSidedTetromino
    ( [ onEvent landE
      $ withBehaviour remainingLettersB
      $ setValueRandomly $ \((), remainingLetters) -> do
          oneSidedTetromino <- randomOneSidedTetromino remainingLetters
          pure oneSidedTetromino
      ]
   ++ [ onEvent rotateE
      $ withBehaviour boardB
      $ withBehaviour piecePosB
      $ changeStateOf #theValue $ \(((), board),piecePos) -> do
          oneSidedTetromino <- get
          let oneSidedTetromino' = rotate oneSidedTetromino
          when (isMoveLegal board $ Piece oneSidedTetromino' piecePos) $ do
            put oneSidedTetromino'
      | (rotate, rotateE) <- [(rotateLeft, ccwE), (rotateRight, cwE)]
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
      | (dir, dirE) <- [(E, rightE), (W, leftE), (S, downE), (S, gravityE)]
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

  let currentPieceB = Piece <$> oneSidedTetrominoB <*> piecePosB
  let worldB = World <$> boardB <*> currentPieceB
  lift $ reactimate (presentWorld window renderer assets <$> worldB <@ timeE)
  lift $ reactimate (Mixer.play (assetsMoveSoundEffect assets) <$ mconcat [leftE, downE, rightE])
  lift $ reactimate (quit <$ escE)
  lift $ reactimate (quit <$ gameOverE)
