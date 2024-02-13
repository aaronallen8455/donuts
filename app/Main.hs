{-# OPTIONS_GHC -fplugin Donuts.Plugin  #-}
module Main where

import           Data.Functor.Identity
import           Donuts.Api

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

blah :: IO Int
blah = do
  i <- getLine
  forL [1::Int,2,3,4,5] $ \ix -> do
    when (show ix == i) $ earlyReturn 9
    print ix
  when (i == "no") $
    earlyReturn 5
  if i == "yes"
     then earlyReturn 2
     else putStrLn "t"
  case i of
    "what" -> earlyReturn 100
    "huh" -> do
      putStrLn "yup"
      earlyReturn 101
    "nope" -> pure ()
    _ -> putStrLn "nah"
  pure 3

k :: Int
k = runIdentity $ do
--   when True (earlyReturn 1)
  forL [(2::Int)..8] $ \i -> do
    when (i == 5) $ earlyReturn 1
  pure 2


s :: Int
s = runIdentity $ do
        when True (earlyReturn 1)
        pure (3 :: Int)

bc :: IO ()
bc = do
  putStrLn "here we go"
  let Mut x = "yo"
  x := "yoyo"
  print x
  forL [(1::Int)..12] $ \i -> do
    print i
    when (i == 7) continueL
    print (i * 2)
    x := x ++ "."
    when (i == 9) breakL
  putStrLn "the end"
  print x
