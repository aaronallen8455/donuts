module Main (main) where

import           Control.Monad (void)
import           Control.Monad.State (runState, get, modify')
import           Control.Monad.Writer (runWriter, tell)
import           Data.Functor.Identity
import           Data.IORef
import           System.IO.Unsafe
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
  , testCase "when 3" when3
  , testCase "case 1" case1
  , testCase "case 2" case2
  , testCase "case 3" case3
  , testCase "forL" for1
  , testCase "nested forL" for2
  , testCase "continue + break" continueBreak
  , testCase "nested continue + break" nestedContinueBreak
  , testCase "repeatL state" repeatLState
  , testCase "mut var 1" mutVar1
  , testCase "mut var 2" mutVar2
  , testCase "mut var 3" mutVar3
  , testCase "mut var 4" mutVar4
  , testCase "mut var 5" mutVar5
  , testCase "mut var 6" mutVar6
  , testCase "mut var 7" mutVar7
  , testCase "whileL 1" whileL1
  , testCase "bind mut 1" bindMut1
  , testCase "laziness" laziness1
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

when3 :: Assertion
when3 =
  let s = runIdentity $ do
        when True $ earlyReturn $ 3 + 9
        pure (3 :: Int)
   in s @?= 12

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

for2 :: Assertion
for2 =
  let s = runIdentity $ do
        forL [(2::Int)..8] $ \i ->
          forL [1..3] $ \j ->
            when (i + j == 5) $ earlyReturn (1 :: Int)
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
        let Mut x = 1 :: Int
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
          forL [(1::Int)..x] $ \i -> do
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

mutVar7 :: Assertion
mutVar7 =
  let s = runIdentity $ do
        let Mut x = 1
        forL "abc" $ \_ -> do
          let Mut y = 2
          y := 3
          x := y
        pure x
   in s @?= (3 :: Int)

whileL1 :: Assertion
whileL1 =
  let s = runIdentity $ do
        let Mut x = 1
        whileL (x < 5) $ do
          x := x + 1
        pure x
   in s @?= (5 :: Int)

bindMut1 :: Assertion
bindMut1 =
  let s = do
        Mut x <- Just 1
        Mut y <- Just (9 :: Int)
        whileL (x < 5) $ do
          Mut z <- Just (10 :: Int)
          x := x + 1
        pure x
   in s @?= Just (5 :: Int)

laziness1 :: Assertion
laziness1 = do
  ref1 <- newIORef False
  ref2 <- newIORef False
  ref3 <- newIORef False
  ref4 <- newIORef False
  ref5 <- newIORef False
  ref6 <- newIORef False

  let !() = runIdentity $ do
            let Mut x = unsafePerformIO (void $ writeIORef ref1 True)
            let Mut !y = unsafePerformIO (void $ writeIORef ref2 True)
            let Mut ~z = unsafePerformIO (void $ writeIORef ref3 True)
            x := unsafePerformIO (void $ writeIORef ref4 True)
            y := unsafePerformIO (void $ writeIORef ref5 True)
            z := unsafePerformIO (void $ writeIORef ref6 True)
            pure ()
  r1 <- readIORef ref1
  r1 @?= False
  r2 <- readIORef ref2
  r2 @?= True
  r3 <- readIORef ref3
  r3 @?= False
  r4 <- readIORef ref4
  r4 @?= False
  r5 <- readIORef ref5
  r5 @?= True
  r6 <- readIORef ref6
  r6 @?= False
