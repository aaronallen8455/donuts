{-# OPTIONS_GHC -fplugin Sugar.Plugin  #-}
module Main where

import           Sugar.Api

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

blah :: IO Int
blah = do
  i <- getLine
  if i == "yes"
     then earlyReturn 2
     else pure ()
  pure 3
