{-# OPTIONS -fplugin Sugar.Plugin #-}
module Main (main) where

import           Data.Functor.Identity
import           Sugar.Api
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "statements"
  [ testCase "if 1" ifStatement1
  , testCase "if 2" ifStatement2
  , testCase "when 1" when1
  , testCase "when 2" when2
  , testCase "case 1" case1
  , testCase "case 2" case2
  , testCase "case 3" case3
  , testCase "forLoop" for1
  ]

ifStatement1 :: Assertion
ifStatement1 =
  let s = runIdentity $ do
        if True
           then do
             earlyReturn (1 :: Int)
           else pure 2
  in s @?= 1

ifStatement2 :: Assertion
ifStatement2 =
  let s = runIdentity $ do
        if True
           then do
             earlyReturn (1 :: Int)
             pure 3
           else pure 2
  in s @?= 1

when1 :: Assertion
when1 =
  let s = runIdentity $ do
        when True (earlyReturn 1)
        pure (3 :: Int)
   in s @?= 1

when2 :: Assertion
when2 =
  let s = runIdentity $ do
        when True $ earlyReturn 1
        pure (3 :: Int)
   in s @?= 1

case1 :: Assertion
case1 =
  let s = runIdentity $ do
        case succ 'a' of
          'a' -> pure (9 :: Int)
          'b' -> do
            earlyReturn 1
          _ -> pure 8
   in s @?= 1

case2 :: Assertion
case2 =
  let s = runIdentity $ do
        case succ 'a' of
          'a' -> pure (9 :: Int)
          'b' -> do
            earlyReturn 1
            pure 2
          _ -> pure 8
   in s @?= 1

case3 :: Assertion
case3 =
  let s = runIdentity $ do
        case succ 'a' of
          'a' -> pure (9 :: Int)
          'b' -> do
            when True $ earlyReturn 1
            pure 2
          _ -> pure 8
   in s @?= 1

for1 :: Assertion
for1 =
  let s = runIdentity $ do
        forLoop [(2::Int)..8] $ \i -> do
          when (i == 5) $ earlyReturn (1 :: Int)
        pure 2
   in s @?= 1
