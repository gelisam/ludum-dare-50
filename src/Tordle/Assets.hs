{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}
{-# OPTIONS -Wno-incomplete-uni-patterns #-}
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
  , assetsGameOverSoundEffect
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
  withMultiple
      [ With $ withFont "assets/clear-sans.regular.ttf" 50
      , With $ withFont "assets/clear-sans.regular.ttf" 24
      ]
      $ \[titleFont, letterFont] -> do
    withMultiple
        [ With $ withSoundEffect "assets/move.wav"
        , With $ withSoundEffect "assets/lose.wav"
        ]
        $ \[moveSoundEffect, gameOverSoundEffect] -> do
      withMultiple
          [ With $ withTextTexture renderer titleFont (V4 0 0 0 255) "Tordle"
          , With $ withTextTexture renderer titleFont (V4 0 0 0 255) "Game Over"
          , With $ withTextTexture renderer titleFont (V4 0 0 0 255) "Congratulations!"
          ]
          $ \[titleTexture, gameOverTexture, winTexture] -> do
        let allChars = '?' : allLetters
        withMultiple
            [ With $ withTextTexture renderer letterFont (V4 0 0 0 255) (Text.singleton c)
            | c <- allChars
            ]
            $ \blackLetterTextures -> do
          withMultiple
              [ With $ withTextTexture renderer letterFont (V4 255 255 255 255) (Text.singleton c)
              | c <- allChars
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
                  = Map.fromList $ zip allChars blackLetterTextures
              , assetsWhiteLetterTextures
                  = Map.fromList $ zip allChars whiteLetterTextures
              , assetsMoveSoundEffect
                  = moveSoundEffect
              , assetsGameOverSoundEffect
                  = gameOverSoundEffect
              , assetsCommonWords
                  = Set.fromList $ lines commonWords
              , assetsAllWords
                  = Set.fromList $ lines allWords
              }
