module Main where

import qualified LudumDare (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  LudumDare.someFunc
