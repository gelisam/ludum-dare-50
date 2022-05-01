{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS -Wno-incomplete-uni-patterns #-}
module Tordle.Assets where

import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import SDL.Video.Renderer.Shim (Renderer)


data Assets = Assets
  { assetsCommonWords
      :: Set String
  , assetsAllWords
      :: Set String
  }
  deriving (Generic)

withAssets
  :: Renderer
  -> (Assets -> IO a)
  -> IO a
withAssets _renderer body = do
  commonWords <- readFile "assets/common-words.txt"
  allWords <- readFile "assets/all-words.txt"
  body $ Assets
    { assetsCommonWords
        = Set.fromList $ lines commonWords
    , assetsAllWords
        = Set.fromList $ lines allWords
    }
