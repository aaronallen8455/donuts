module Main (main) where

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Functor.Identity
import           Donuts.Api
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
  , testCase "forL" for1
  , testCase "continue + break" continueBreak
  , testCase "nested continue + break" nestedContinueBreak
  , testCase "repeatL state" repeatLState
  , testCase "mut var 1" mutVar1
  , testCase "mut var 2" mutVar2
  , testCase "mut var 3" mutVar3
  , testCase "mut var 4" mutVar4
  , testCase "mut var 5" mutVar5
  , testCase "mut var 6" mutVar6
  , testCase "whileL 1" whileL1
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
        forL [(2::Int)..8] $ \i -> do
          when (i == 5) $ earlyReturn (1 :: Int)
        pure 2
   in s @?= 1

continueBreak :: Assertion
continueBreak =
  let (_, s) = runWriter $ do
        tell "start"
        forL [(1::Int)..12] $ \i -> do
          tell $ show i
          when (i == 7) continueL
          tell $ show (i * 2)
          when (i == 9) breakL
        tell "end"
   in s @?= "start122436485106127816918end"

nestedContinueBreak :: Assertion
nestedContinueBreak =
  let (_, s) = runWriter $ do
        tell "start"
        forL [(1::Int)..12] $ \i -> do
          tell $ show i
          when (i == 7) continueL
          forL [1 .. i] $ \j -> do
            tell $ show j
            if even j
               then continueL
               else do
                 when (j + i > 8) breakL
                 tell "."
          tell $ show (i * 2)
          when (i == 9) breakL
        tell "end"
   in s @?= "start11.221.2431.23.641.23.4851.23.451061.2312781169118end"

repeatLState :: Assertion
repeatLState =
  let (_, s) = (`runState` 1) $ do
        repeatL $ do
          modify' succ
          x <- get
          when (x == 3) breakL
   in s @?= (3 :: Int)

mutVar1 :: Assertion
mutVar1 =
  let s = runIdentity $ do
        let Mut x = 2
        x := x + 1
        pure x
   in s @?= (3 :: Int)

mutVar2 :: Assertion
mutVar2 =
  let s = runIdentity $ do
        let Mut x = 2
        let Mut y = x
        y := y + 1
        pure y
   in s @?= (3 :: Int)

mutVar3 :: Assertion
mutVar3 =
  let s = runIdentity $ do
        let Mut x = 1
        repeatL $ do
          when (x == 5) $ earlyReturn 99
          x := x + 1
        pure 100
   in s @?= (99 :: Int)

mutVar4 :: Assertion
mutVar4 =
  let s = runIdentity $ do
        let Mut x = 1
        repeatL $ do
          forL [1..x] $ \i -> do
            x := x + 1
          when (x == 8) $ earlyReturn 99
        pure 100
   in s @?= (99 :: Int)

mutVar5 :: Assertion
mutVar5 =
  let s = runIdentity $ do
        let Mut x = 1
        let y = x + 1
        x := 99
        pure y
   in s @?= (2 :: Int)

mutVar6 :: Assertion
mutVar6 =
  let s = runIdentity $ do
        let Mut x = 1
        let Mut y = 2
        forL "abc" $ \_ -> do
          y := x
          x := 3
        pure y
   in s @?= (3 :: Int)

whileL1 :: Assertion
whileL1 =
  let s = runIdentity $ do
        let Mut x = 1
        whileL (x < 5) $ do
          x := x + 1
        pure x
   in s @?= (5 :: Int)
