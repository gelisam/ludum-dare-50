{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}
module Tordle.Assets where

import Data.Function.Extra (With(..), withMultiple)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import SDL (V4(..))
import SDL.Video.Renderer (Renderer, Texture)
import SDL.Extra


data Assets = Assets
  { assetsBlackLetterTextures
      :: Map Char Texture
  , assetsTitleTexture
      :: Texture
  , assetsMoveSoundEffect
      :: SoundEffect
  , assetsWhiteLetterTextures
      :: Map Char Texture
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
          withMultiple
              [ With $ withTextTexture renderer letterFont (V4 0 0 0 255) (Text.singleton c)
              | c <- ['A'..'Z']
              ]
              $ \blackLetterTextures -> do
            withMultiple
                [ With $ withTextTexture renderer letterFont (V4 255 255 255 255) (Text.singleton c)
                | c <- ['A'..'Z']
                ]
                $ \whiteLetterTextures -> do
              body $ Assets
                { assetsBlackLetterTextures
                    = Map.fromList $ zip ['A'..'Z'] blackLetterTextures
                , assetsTitleTexture
                    = titleTexture
                , assetsMoveSoundEffect
                    = moveSoundEffect
                , assetsWhiteLetterTextures
                    = Map.fromList $ zip ['A'..'Z'] whiteLetterTextures
                }