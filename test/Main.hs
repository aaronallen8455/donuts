module Main (main) where

import           Control.Monad.Writer
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
  , testCase "continue + break" continueBreak
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

continueBreak :: Assertion
continueBreak =
  let (_, s) = runWriter $ do
        tell "start"
        forLoop [(1::Int)..12] $ \i -> do
          tell $ show i
          when (i == 7) continueL
          tell $ show (i * 2)
          when (i == 9) breakL
        tell "end"
   in s @?= "start122436485106127816918end"
