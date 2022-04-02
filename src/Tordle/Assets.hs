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
  { assetsTitleTexture
      :: Texture
  , assetsLetterTextures
      :: Map Char Texture
  , assetsMoveSoundEffect
      :: SoundEffect
  }

withAssets
  :: Renderer
  -> (Assets -> IO a)
  -> IO a
withAssets renderer body = do
  withFont "assets/clear-sans.regular.ttf" 24 $ \font -> do
    withSoundEffect "assets/move.wav" $ \moveSoundEffect -> do
      withTextTexture renderer font (V4 0 255 255 255) "Tordle" $ \titleTexture -> do
        withMultiple
            [ With $ withTextTexture renderer font (V4 0 255 255 255) (Text.singleton c)
            | c <- ['A'..'Z']
            ]
            $ \charTextures -> do
          body $ Assets
            { assetsTitleTexture
                = titleTexture
            , assetsLetterTextures
                = Map.fromList $ zip ['A'..'Z'] charTextures
            , assetsMoveSoundEffect
                = moveSoundEffect
            }
