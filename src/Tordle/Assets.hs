{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}
module Tordle.Assets where

import Data.Function.Extra (With(..), withMultiple)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import SDL (V4(..))
import SDL.Video.Renderer (Renderer, Texture)
import SDL.Extra
import Tordle.Model


data Assets = Assets
  { assetsTitleTexture
      :: Texture
  , assetsGameOverTexture
      :: Texture
  , assetsWinTexture
      :: Texture
  , assetsBlackLetterTextures
      :: Map Char Texture
  , assetsWhiteLetterTextures
      :: Map Char Texture
  , assetsMoveSoundEffect
      :: SoundEffect
  , assetsCommonWords
      :: Set String
  , assetsAllWords
      :: Set String
  }

withAssets
  :: Renderer
  -> (Assets -> IO a)
  -> IO a
withAssets renderer body = do
  withFont "assets/clear-sans.regular.ttf" 50 $ \titleFont -> do
    withFont "assets/clear-sans.regular.ttf" 24 $ \letterFont -> do
      withSoundEffect "assets/move.wav" $ \moveSoundEffect -> do
        withTextTexture renderer titleFont (V4 0 0 0 255) "Tordle" $ \titleTexture -> do
          withTextTexture renderer titleFont (V4 0 0 0 255) "Game Over" $ \gameOverTexture -> do
            withTextTexture renderer titleFont (V4 0 0 0 255) "Congratulations!" $ \winTexture -> do
              withMultiple
                  [ With $ withTextTexture renderer letterFont (V4 0 0 0 255) (Text.singleton c)
                  | c <- allLetters
                  ]
                  $ \blackLetterTextures -> do
                withMultiple
                    [ With $ withTextTexture renderer letterFont (V4 255 255 255 255) (Text.singleton c)
                    | c <- allLetters
                    ]
                    $ \whiteLetterTextures -> do
                  commonWords <- readFile "assets/common-words.txt"
                  allWords <- readFile "assets/all-words.txt"
                  body $ Assets
                    { assetsTitleTexture
                        = titleTexture
                    , assetsGameOverTexture
                        = gameOverTexture
                    , assetsWinTexture
                        = winTexture
                    , assetsBlackLetterTextures
                        = Map.fromList $ zip allLetters blackLetterTextures
                    , assetsWhiteLetterTextures
                        = Map.fromList $ zip allLetters whiteLetterTextures
                    , assetsMoveSoundEffect
                        = moveSoundEffect
                    , assetsCommonWords
                        = Set.fromList $ lines commonWords
                    , assetsAllWords
                        = Set.fromList $ lines allWords
                    }
