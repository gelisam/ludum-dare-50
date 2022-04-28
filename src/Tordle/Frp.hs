{-# LANGUAGE DataKinds, OverloadedLabels, TypeApplications, LambdaCase, RecursiveDo #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Tordle.Frp where

import Control.Lens
import Control.Monad (guard, when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Char as Char
import Data.Foldable
import Data.Function.Extra
import Data.Generics.Labels ()
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Traversable
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..), _x, _y)
import Reactive.Banana.Combinators
import Reactive.Banana.Extra
import Reactive.Banana.Frameworks
import qualified SDL.Stub as SDL
import qualified SDL.Mixer.Stub as Mixer
import SDL.Video.Stub (Window)
import SDL.Video.Renderer.Stub (Renderer)
import System.Random.Stateful.Stub (StdGen)
import Tordle.Assets
import Tordle.Dir
import Tordle.Draw
import Tordle.Guess
import Tordle.Model
import Tordle.Share
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
  let keyRE     = () <$ filterE (hasKeycode SDL.KeycodeR) keyPressE
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
    [ onEvent resetE             $ setValue $ \() -> True
    , onEvent switchToChooseE    $ setValue $ \() -> True
    , onEvent switchToPlaceE     $ setValue $ \() -> False
    , onEvent switchToEndScreenE $ setValue $ \() -> False
    ]
  canPlaceB <- changingB False
    [ onEvent resetE             $ setValue $ \() -> False
    , onEvent switchToChooseE    $ setValue $ \() -> False
    , onEvent switchToPlaceE     $ setValue $ \() -> True
    , onEvent switchToEndScreenE $ setValue $ \() -> False
    ]
  canResetB <- changingB False
    [ onEvent resetE             $ setValue $ \() -> False
    , onEvent switchToChooseE    $ setValue $ \() -> False
    , onEvent switchToPlaceE     $ setValue $ \() -> False
    , onEvent switchToEndScreenE $ setValue $ \() -> True
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
      resetE
        = whenE canResetB keyRE

  timeB <- stepper 0 timeE
  let delayE
        :: MonadMoment m
        => Double
        -> Event a
        -> m (Event (Double, a), Event a)
      delayE delay startE = mdo
        nextTickB <- changingB Nothing
          [ onEvent delayedE
          $ setValue $ \_
         -> Nothing
          , onEvent startE
          $ withBehaviour timeB
          $ setValue $ \(a, t)
         -> Just (a, t + delay)
          ]
        let easingE
              = givenEvent timeE
              $ withBehaviour nextTickB
              $ maybeKeepIt $ \(t, maybeNextTick) -> do
                  (a, nextTick) <- maybeNextTick
                  let t0 = nextTick - delay
                  let fraction = ((t - t0) / delay) `min` 1
                  pure (fraction, a)
        let delayedE
              = givenEvent timeE
              $ withBehaviour nextTickB
              $ maybeKeepIt $ \(t, maybeNextTick) -> do
                  (a, nextTick) <- maybeNextTick
                  guard (t >= nextTick)
                  pure a
        pure (easingE, delayedE)
  (_, gravityTickE) <- delayE 1 (userTriesToMoveDownE <> gravityTickE <> switchToPlaceE)

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

  initialAnalyzedRowsE <- changingRandomlyE (error "initialAnalyzedRowsE: t=0 value")
    [ onEvent landingBlocksE
    $ withSimultaneousEvent boardWithWildBlocksE
    $ withBehaviour correctWordB
    $ withBehaviour knownLettersB
    $ setValueRandomly $ \(((landingBlocks, board), correctWord), knownLetters) -> do
        let completedRows
              :: [CInt]
            completedRows
              = filter (isRowComplete board)
              $ Set.toList
              $ Set.fromList
              $ fmap (view _y)
              $ Map.keys
              $ landingBlocks
        for completedRows $ \y -> do
          analyzedRow <- analyzeCompletedRow assets correctWord knownLetters board y
          pure (y, analyzedRow)
    ]
  let laterAnalyzedRowsE
        = givenEvent rowAnimationCompleteE
        $ withBehaviour remainingAnalyzedRowsB
        $ transformIt $ \(_, analyzedRows)
       -> analyzedRows
  let analyzedRowsE
        :: Event [(CInt, AnalyzedRow)]
      analyzedRowsE
        = initialAnalyzedRowsE
       <> laterAnalyzedRowsE
  let analyzedRowE
        = givenEvent analyzedRowsE
        $ maybeKeepIt $ \analyzedRows -> do
            case analyzedRows of
              analyzedRow:_ -> do
                pure $ analyzedRow
              [] -> do
                Nothing
  remainingAnalyzedRowsB <- changingB []
    [ onEvent analyzedRowsE
    $ setValue $ \analyzedRows
   -> analyzedRows
    , onEvent analyzedRowE
    $ changeValue $ \(_, analyzedRows)
   -> drop 1 analyzedRows
    , onEvent rowActionsE
    $ changeValue $ \(rowActions, analyzedRows)
   -> Map.toList
    $ performRowActions id rowActions
    $ Map.fromList
    $ analyzedRows
    ]

  let completionAnimationBeganE
        = givenEvent analyzedRowE
        $ maybeKeepIt $ \(y, analyzedRow) -> do
            guard (rowHadWilds analyzedRow)
            pure (y, analyzedRow)
  let noCompletionAnimationNeededE
       = givenEvent analyzedRowE
       $ maybeKeepIt $ \(y, analyzedRow) -> do
           guard (not $ rowHadWilds analyzedRow)
           pure (y, analyzedRow)
  (_, initialCompletionE) <- delayE 0 completionAnimationBeganE
  let laterCompletionE
        = givenEvent completionShownE
        $ maybeKeepIt $ \(y, analyzedRow) -> do
            guard (not $ null $ rowExtraCompletions analyzedRow)
            let analyzedRow' = over #rowExtraCompletions (drop 1) analyzedRow
            pure (y, analyzedRow')
  let completionAnimationCompleteE
        = givenEvent completionShownE
        $ maybeKeepIt $ \(y, analyzedRow) -> do
            guard (null $ rowExtraCompletions analyzedRow)
            pure (y, analyzedRow)
  let completionE
        :: Event (CInt, AnalyzedRow)
      completionE
        = unionWith (error "completionE: simultaneous occurrences")
            initialCompletionE
            laterCompletionE
  (_, completionShownE) <- delayE 0.2 completionE
  let completionPhaseOverE
        = unionWith (error "completionPhaseOverE: simultaneous occurrences")
             completionAnimationCompleteE
             noCompletionAnimationNeededE

  let guessE
        = givenEvent completionPhaseOverE
        $ maybeKeepIt $ \(y, analyzedRow) -> do
            let completion = rowCompletion analyzedRow
            coloring <- rowColoring analyzedRow
            pure (y, AnalyzedGuess completion coloring)
  let nonWordE
        = givenEvent completionPhaseOverE
        $ maybeKeepIt $ \(y, analyzedRow) -> do
            let completion = rowCompletion analyzedRow
            guard (rowColoring analyzedRow == Nothing)
            pure (y, completion)

  (_, coloringAnimationBeganE) <- delayE 0.3 guessE
  let initialColoringE
        = givenEvent coloringAnimationBeganE
        $ transformIt $ \(y, analyzedRow)
       -> (0, y, analyzedRow)
  let laterColoringE
        = givenEvent coloringShownE
        $ maybeKeepIt $ \(x, y, analyzedRow) -> do
            guard (x < mAIN_BOARD_SIZE^._x - 1)
            pure (x+1, y, analyzedRow)
  let coloringAnimationCompleteE
        = givenEvent coloringShownE
        $ maybeKeepIt $ \(x, y, analyzedRow) -> do
            guard (x == mAIN_BOARD_SIZE^._x - 1)
            pure (y, analyzedRow)
  let coloringE
        = unionWith (error "coloringE: simultaneous occurrences")
            initialColoringE
            laterColoringE
  let greyLetterE
        = givenEvent coloringE
        $ maybeKeepIt $ \(x, _, analyzedGuess) -> do
            let coloring = guessColoring analyzedGuess
            let guessResult = coloring !! fromIntegral x
            guard (guessResult == Grey)
            pure x
  let yellowLetterE
        = givenEvent coloringE
        $ maybeKeepIt $ \(x, _, analyzedGuess) -> do
            let coloring = guessColoring analyzedGuess
            let guessResult = coloring !! fromIntegral x
            guard (guessResult == Yellow)
            pure x
  let greenLetterE
        = givenEvent coloringE
        $ maybeKeepIt $ \(x, _, analyzedGuess) -> do
            let coloring = guessColoring analyzedGuess
            let guessResult = coloring !! fromIntegral x
            guard (guessResult == Green)
            pure x
  (coloringEasingE, coloringShownE) <- delayE 0.3 coloringE

  let nonWordAnimationBeganE
        = nonWordE
  (nonWordEasingE, nonWordAnimationCompleteE) <- delayE 0.8 nonWordAnimationBeganE

  let moveToBottomE
        = givenEvent coloringAnimationCompleteE
        $ transformIt $ \(y, _)
       -> (y, MoveRowToBottom)
  let deleteRowE
        = givenEvent nonWordAnimationCompleteE
        $ transformIt $ \(y, _)
       -> (y, DeleteRow)
  let nextRowActionE
        = unionWith (error "rowActionsE: simultaneous occurrences")
            moveToBottomE
            deleteRowE
  (_, rowAnimationBeganE) <- delayE 0 nextRowActionE
  (rowActionEasingE, rowAnimationAlmostCompleteE) <- delayE 0.4 rowAnimationBeganE
  let rowActionE
        = rowAnimationAlmostCompleteE
  let rowActionsE
        = givenEvent rowActionE
        $ transformIt $ \(y, rowAction)
       -> Map.singleton y rowAction
  (_, rowAnimationCompleteE) <- delayE 0 rowAnimationAlmostCompleteE
  let nextWinAnimationCompleteE
        = whenE ( (&&)
              <$> ((== Win) <$> worldStatusB)
              <*> (null <$> remainingAnalyzedRowsB)
                )
        $ (() <$ rowAnimationCompleteE)
  (_, winAnimationCompleteE) <- delayE 0 nextWinAnimationCompleteE

  alphabetColoringB <- changingB Map.empty
    [ onEvent resetE $ setValue $ \() -> Map.empty
    , onEvent coloringAnimationCompleteE
    $ changeState $ \(_, analyzedGuess) -> do
        let guess = guessCompletion analyzedGuess
        let coloring = guessColoring analyzedGuess
        for_ (zip guess coloring) $ \(letter, guessResult) -> do
          modify $ Map.insertWith max letter guessResult
    ]
  knownLettersB <- changingB Map.empty
    [ onEvent resetE $ setValue $ \() -> Map.empty
    , onEvent guessE
    $ changeState $ \(_, analyzedGuess) -> do
        let guess = guessCompletion analyzedGuess
        let coloring = guessColoring analyzedGuess
        for_ (zip3 xCoordinates guess coloring) $ \(x, letter, guessResult) -> do
          when (guessResult == Green) $ do
            modify $ Map.insert x letter
    ]

  firstCorrectWord <- randomWord assets
  correctWordB <- changingRandomlyB firstCorrectWord
    [ onEvent resetE
    $ setValueRandomly $ \() -> do
        randomWord assets
    ]

  firstFixedTetrominoIndex <- randomIndex fixedTetrominos
  fixedTetrominoIndexE <- changingRandomlyE firstFixedTetrominoIndex
    [ onEvent resetE
    $ setValueRandomly $ \() -> do
        randomIndex fixedTetrominos
    , onEvent landE
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
    ( [ onEvent resetE $ setValue $ \() -> firstPos
      , onEvent landE  $ setValue $ \() -> firstPos
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
        = givenEvent completionE
        $ withBehaviour boardB
        $ transformIt $ \((y, analyzedRow), board)
       -> flip execState board $ do
            let guess = head (rowExtraCompletions analyzedRow ++ [rowCompletion analyzedRow])
            for_ (zip [0..] guess) $ \(x, letter) -> do
              ix (V2 x y) . #blockLabel .= Letter letter
  let boardWithShakingBlocksE
        = givenEvent nonWordEasingE
        $ withBehaviour boardB
        $ transformIt $ \((t, (y, _)), board)
       -> flip execState board $ do
            for_ xCoordinates $ \x -> do
              ix (V2 x y) . #blockOffset . _x .= 0.25 * sin (16 * t * pi)
  let boardWithColoredBlocksE
        = givenEvent coloringEasingE
        $ withBehaviour boardB
        $ transformIt $ \((t, (x, y, analyzedGuess)), board)
       -> flip execState board $ do
            let coloring = guessColoring analyzedGuess
            let guessResult = coloring !! fromIntegral x
            ix (V2 x y) . #blockStatus .= guessStatus guessResult
            ix (V2 x y) . #blockOffset . _y .= - (1 - (2*t-1)**2)
  let boardWithMovingRowsE
        = givenEvent rowActionEasingE
        $ withBehaviour boardB
        $ transformIt $ \((t, (y, rowAction)), board)
       -> flip execState board $ do
            each . #blockZIndex .= 0
            for_ xCoordinates $ \x -> do
              ix (V2 x y) . #blockZIndex .= 1
            id %= animateRowActions (Map.singleton y rowAction) t
  let boardWithReorderedRowsE
        = givenEvent rowAnimationCompleteE
        $ withBehaviour boardB
        $ transformIt $ \((y, rowAction), board)
       -> resetAllOffsets
        $ performRowActions _y (Map.singleton y rowAction) board
  boardB <- changingB Map.empty
    [ onEvent resetE $ setValue $ \() -> Map.empty
    , onEvent boardWithWildBlocksE $ setValue id
    , onEvent boardWithLetterBlocksE $ setValue id
    , onEvent boardWithShakingBlocksE $ setValue id
    , onEvent boardWithColoredBlocksE $ setValue id
    , onEvent boardWithMovingRowsE $ setValue id
    , onEvent boardWithReorderedRowsE $ setValue id
    ]

  let gameOverE
        = givenEvent landingBlocksE
        $ maybeKeepIt $ \landingBlocks -> do
            guard (isGameOver landingBlocks)
  let winE
        = givenEvent analyzedRowE
        $ maybeKeepIt $ \(_, analyzedRow) -> do
            coloring <- rowColoring analyzedRow
            guard $ all (== Green) coloring
  worldStatusB <- changingB Playing
    [ onEvent resetE    $ setValue $ \() -> Playing
    , onEvent gameOverE $ setValue $ \() -> GameOver
    , onEvent winE      $ setValue $ \() -> Win
    ]
  let maybeSolutionB
        = go <$> correctWordB <*> worldStatusB
        where
          go correctWord worldStatus = do
            guard (worldStatus == GameOver)
            pure correctWord

  playerKnowsHowToPlaceBlocksB <- changingB False
    [ onEvent landE $ setValue $ \() -> True
    ]
  playerKnowsHowToPlayAgainB <- changingB False
    [ onEvent resetE $ setValue $ \() -> True
    ]
  playerKnowsHowToChangeShapeB <- changingB False
    [ onEvent (nextShapeE <> prevShapeE) $ setValue $ \() -> True
    ]
  maybeHelpTextB <- changingB (Just HelpGuessLetter)
    [ onEvent nonWordAnimationBeganE
    $ setValue $ \_ -> Just HelpNotAWord
    , onEvent nonWordAnimationCompleteE
    $ setValue $ \_ -> Nothing
    , onEvent (whenE (not <$> playerKnowsHowToPlaceBlocksB) pickLetterE)
    $ setValue $ \_ -> Just HelpPlaceBlock
    , onEvent (whenE ((== Just HelpPlaceBlock) <$> maybeHelpTextB) landE)
    $ setValue $ \_ -> Nothing
    , onEvent (whenE (not <$> playerKnowsHowToChangeShapeB) gameOverE)
    $ setValue $ \_ -> Just HelpChangeShape
    , onEvent (whenE (not <$> playerKnowsHowToPlayAgainB) switchToEndScreenE)
    $ setValue $ \_ -> Just HelpPlayAgain
    , onEvent resetE
    $ setValue $ \_ -> Nothing
    ]

  let currentPieceB
          = Piece
        <$> oneSidedTetrominoB
        <*> piecePosB
  let worldB
          = World
        <$> worldStatusB
        <*> maybeSolutionB
        <*> maybeHelpTextB
        <*> alphabetColoringB
        <*> boardB
        <*> currentPieceB
  lift $ reactimate (presentWorld window renderer assets <$> worldB <@ timeE)

  lift $ reactimate ( Mixer.play (assets ^?! #assetsSoundEffects . ix SoundMove)
                   <$ mconcat [tryMoveLeftE, tryMoveRightE, userTriesToMoveDownE]
                    )
  lift $ reactimate ( Mixer.play (assets ^?! #assetsSoundEffects . ix SoundLand)
                   <$ landE
                    )
  lift $ reactimate ( Mixer.play (assets ^?! #assetsSoundEffects . ix SoundNotAWord)
                   <$ nonWordE
                    )
  lift $ reactimate ( Mixer.play (assets ^?! #assetsSoundEffects . ix SoundGreyLetter)
                   <$ greyLetterE
                    )
  lift $ reactimate ( Mixer.play (assets ^?! #assetsSoundEffects . ix SoundYellowLetter)
                   <$ yellowLetterE
                    )
  lift $ reactimate ( (\x -> Mixer.play (assets ^?! #assetsSoundEffects . ix (SoundGreenLetter x)))
                  <$> greenLetterE
                    )
  lift $ reactimate ( Mixer.play (assets ^?! #assetsSoundEffects . ix SoundGameOver)
                   <$ gameOverE
                    )
  lift $ reactimate (printSharingText <$> boardB <@ winAnimationCompleteE)

  lift $ reactimate (quit <$ keyEscE)
