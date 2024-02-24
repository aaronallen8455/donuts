{-# OPTIONS_GHC -fplugin Donuts.Plugin -O2  #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import           Data.Foldable
import           Control.Monad.State
import           Data.Functor.Identity
import           Donuts.Api

main :: IO ()
main = do
  print $ fib2a 100000

blah :: IO Int
blah = do
  i <- getLine
  Mut y <- getLine
  y := "user entered: " <> y
  putStrLn y
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

ac :: IO ()
ac = do
  let Mut strings = []
  repeatL $ do
    inp <- getLine
    when (inp == "stop") breakL
    strings := inp : strings
    print strings

zz :: IO ()
zz = do
  i <- getLine
  case i of
    "..." -> do
      let Mut x = True
      x := False
      print x
    _ -> pure ()

fib1 :: Int -> Int
fib1 n = runIdentity $ do
  let Mut a = 0
  let Mut b = 1
  let Mut i = 0
  whileL (i < n) do
    let !c = b
    b := a + b
    a := c
    i := i + 1
  pure b

fib2 :: Int -> Int
fib2 n = runIdentity $ do
  let Mut a = 0
  let Mut b = 1
  forL [1..n] $ \_ -> do
    let !c = b
    b := a + b
    a := c
  pure b

fib2a :: Int -> Int
fib2a n = runIdentity do
  let Mut x = (0, 1)
  forL [1..n] \_ -> do
    let (!a, !b) = x
    x := (b, a + b)
  pure $ snd x

fib3 :: [Int]
fib3 = 0 : 1 : zipWith (+) fib3 (drop 1 fib3)

fib4 :: Int -> Int
fib4 n = fib3 !! n

fib5 :: Int -> Int
fib5 n = (`evalState` (0,1,0)) do
  let go = do
        (a,b,i) <- get
        if i == n
           then pure b
           else do
             put (b, a+b, i+1)
             go
  go

fib6 :: Int -> Int
fib6 n = (`evalState` (0,1)) do
  for_ [1..n] $ \_ -> do
    (!a,!b) <- get
    put (b, a+b)
  (a,b) <- get
  pure b

extendedGcd :: Int -> Int -> (Int, Int, Int, Int, Int)
extendedGcd a b = runIdentity $ do
  let Mut rTup = (a, b)
  let Mut sTup = (1, 0)
  let Mut tTup = (0, 1)

  whileL (snd rTup /= 0) $ do
    let quotient = uncurry div rTup
    rTup := (snd rTup, fst rTup - quotient * snd rTup)
    sTup := (snd sTup, fst sTup - quotient * snd sTup)
    tTup := (snd tTup, fst tTup - quotient * snd tTup)

  pure (fst sTup, fst tTup, fst rTup, snd tTup, snd sTup)

extendedGcd' :: Int -> Int -> (Int, Int, Int, Int, Int)
extendedGcd' a b = go a b 1 0 0 1
  where
    go oldR 0 oldS _s oldT t = (oldS, oldT, oldR, t, s)
    go !oldR !r !oldS !s !oldT !t =
      let quotient = div oldR r
       in go r (oldR - quotient * r)
             s (oldS - quotient * s)
             t (oldT - quotient * t)
