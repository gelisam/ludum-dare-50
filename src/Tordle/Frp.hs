{-# LANGUAGE DataKinds, OverloadedLabels, TypeApplications, ImportQualifiedPost, LambdaCase, RecursiveDo #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Tordle.Frp where

import Control.Lens
import Control.Monad (guard, when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Char qualified as Char
import Data.Foldable
import Data.Function.Extra
import Data.Generics.Labels ()
import Data.Map (Map)
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

  let hasKeycode keycode keysym
        = SDL.keysymKeycode keysym == keycode
  let hasShift
        = anyP [SDL.keyModifierLeftShift, SDL.keyModifierRightShift]
        . SDL.keysymModifier
  let keyUpE    = () <$ filterE (hasKeycode SDL.KeycodeUp) keyPressE
  let keyLeftE  = () <$ filterE (hasKeycode SDL.KeycodeLeft) keyPressE
  let keyDownE  = () <$ filterE (hasKeycode SDL.KeycodeDown) keyPressE
  let keyRightE = () <$ filterE (hasKeycode SDL.KeycodeRight) keyPressE
  let keyTabE   = () <$ filterE (allP [hasKeycode SDL.KeycodeTab, not . hasShift]) keyPressE
  let keyUntabE = () <$ filterE (allP [hasKeycode SDL.KeycodeTab, hasShift]) keyPressE
  let keyEscE   = () <$ filterE (hasKeycode SDL.KeycodeEscape) keyReleaseE

  let keyLetterE
        = givenEvent keyPressE
        $ maybeKeepIt $ \keysym -> do
            let keycode = fromIntegral $ SDL.unwrapKeycode $ SDL.keysymKeycode keysym
            guard (keycode >= Char.ord 'a' && keycode <= Char.ord 'z')
            pure $ Char.toUpper $ Char.chr keycode

  let switchToChooseE
        = landE
      switchToPlaceE
        = () <$ pickLetterE
      switchToEndScreenE
        = gameOverE
       <> winE
  canChooseB <- changingB True
    [ onEvent switchToChooseE    $ setValue $ \() -> True
    , onEvent switchToPlaceE     $ setValue $ \() -> False
    , onEvent switchToEndScreenE $ setValue $ \() -> False
    ]
  canPlaceB <- changingB False
    [ onEvent switchToChooseE    $ setValue $ \() -> False
    , onEvent switchToPlaceE     $ setValue $ \() -> True
    , onEvent switchToEndScreenE $ setValue $ \() -> False
    ]
  let pickLetterE
        = whenE canChooseB keyLetterE
      nextShapeE
        = whenE canChooseB keyTabE
      prevShapeE
        = whenE canChooseB keyUntabE
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
    , onEvent switchToPlaceE       $ withBehaviour timeB $ setValue $ \((),t) -> t + 1
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

      mkQuestionMarkTetromino
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

      improveLabels
        :: Map CInt Char
        -> [Label]
        -> [Label]
      improveLabels knownLetters
        = iover itraversed $ \i label
       -> case (label, Map.lookup (fromIntegral i) knownLetters) of
            (Wild, Just knownLetter)
              -> Letter knownLetter
            _
              -> label

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
    $ withBehaviour knownLettersB
    $ setValueRandomly $ \((completedRows, board), knownLetters) -> do
        for completedRows $ \y -> do
          let getLabel x y = board ^?! ix (V2 x y) . #blockLabel
          let labels = map (flip getLabel y) xCoordinates
          let improvedLabels = improveLabels knownLetters labels
          randomCompatibleWord assets improvedLabels >>= \case
            Just completion -> do
              pure completion
            Nothing -> do
              randomCompatibleWord assets labels >>= \case
                Just completion -> do
                  pure completion
                Nothing -> do
                  randomCompatibleGibberish labels
    ]
  let areRealWordsE
        = givenEvent guessesE
        $ transformIt
        $ fmap (isRealWord assets)
  let coloringsE
        = givenEvent guessesE
        $ withSimultaneousEvent areRealWordsE
        $ transformIt $ \(guesses, areRealWords)
       -> flip fmap (zip guesses areRealWords) $ \(guess, isRealWord_)
       -> if isRealWord_
          then Just <$> analyzeGuess correctWord guess
          else replicate 5 Nothing
  alphabetColoringB <- changingB Map.empty
    [ onEvent guessesE
    $ withSimultaneousEvent coloringsE
    $ changeState $ \(guesses, colorings) -> do
        for_ (zip guesses colorings) $ \(guess, maybeGuessResults) -> do
          for_ (zip guess maybeGuessResults) $ \(letter, maybeGuessResult) -> do
            for_ maybeGuessResult $ \guessResult -> do
              modify $ Map.insertWith max letter guessResult
    ]
  knownLettersB <- changingB Map.empty
    [ onEvent guessesE
    $ withSimultaneousEvent coloringsE
    $ changeState $ \(guesses, colorings) -> do
        for_ (zip guesses colorings) $ \(guess, maybeGuessResults) -> do
          for_ (zip3 xCoordinates guess maybeGuessResults) $ \(x, letter, maybeGuessResult) -> do
            when (maybeGuessResult == Just Green) $ do
              modify $ Map.insert x letter
    ]

  correctWord <- randomWord assets

  firstFixedTetrominoIndex <- randomIndex fixedTetrominos
  fixedTetrominoIndexE <- changingRandomlyE firstFixedTetrominoIndex
    [ onEvent landE
    $ setValueRandomly $ \() -> do
        randomIndex fixedTetrominos
    , onEvent nextShapeE
    $ changeValueOf #theValue $ \((), i)
   -> (i + 1) `mod` length fixedTetrominos
    , onEvent prevShapeE
    $ changeValueOf #theValue $ \((), i)
   -> (i + length fixedTetrominos - 1) `mod` length fixedTetrominos
    ]

  let firstFixedTetromino
        = fixedTetrominos !! firstFixedTetrominoIndex
  let fixedTetrominoE
        = givenEvent fixedTetrominoIndexE
        $ transformIt $ \i
       -> fixedTetrominos !! i

  let firstOneSidedTetromino = mkQuestionMarkTetromino firstFixedTetromino
  oneSidedTetrominoB <- changingB firstOneSidedTetromino
    ( [ onEvent fixedTetrominoE
      $ setValue $ \fixedTetromino -> do
          mkQuestionMarkTetromino fixedTetromino
      , onEvent pickLetterE
      $ changeState $ \letter -> do
          mapped . #_Letter .= letter
      ]
   ++ [ onEvent rotateE
      $ withBehaviour boardB
      $ withBehaviour piecePosB
      $ changeState $ \(((), board),piecePos) -> do
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
            for_ (zip completedRows colorings) $ \(y, maybeGuessResults) -> do
              for_ (zip [0..] maybeGuessResults) $ \(x, maybeGuessResult) -> do
                for_ maybeGuessResult $ \guessResult -> do
                  ix (V2 x y) . #blockStatus .= guessStatus guessResult
  let boardWithReorderedRowsE
        = givenEvent completedRowsE
        $ withSimultaneousEvent areRealWordsE
        $ withSimultaneousEvent boardWithColoredBlocksE
        $ transformIt $ \((completedRows, areRealWords), board)
       -> flip performRowActions board
        $ Map.fromList
        $ flip fmap (zip completedRows areRealWords) $ \(y, isRealWord_)
       -> (y, if isRealWord_ then MoveRowToBottom else DeleteRow)
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
            guard $ any (all (== Just Green)) colorings
  worldStatusB <- changingB Playing
    [ onEvent gameOverE $ setValue $ \() -> GameOver
    , onEvent winE      $ setValue $ \() -> Win
    ]

  let currentPieceB = Piece <$> oneSidedTetrominoB <*> piecePosB
  let worldB = World <$> worldStatusB <*> alphabetColoringB <*> boardB <*> currentPieceB
  lift $ reactimate (presentWorld window renderer assets <$> worldB <@ timeE)

  lift $ reactimate ( Mixer.play (assetsMoveSoundEffect assets)
                   <$ mconcat [tryMoveLeftE, tryMoveRightE, userTriesToMoveDownE]
                    )
  lift $ reactimate ( Mixer.play (assetsGameOverSoundEffect assets)
                   <$ gameOverE
                    )

  lift $ reactimate (quit <$ keyEscE)
