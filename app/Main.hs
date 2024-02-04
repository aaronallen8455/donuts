{-# OPTIONS_GHC -fplugin Sugar.Plugin  #-}
module Main where

import           Data.Functor.Identity
import           Sugar.Api

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

-- blah :: IO Int
-- blah = do
--   i <- getLine
--   forLoop [1::Int,2,3,4,5] $ \ix -> do
--     when (show ix == i) $ earlyReturn 9
--     print ix
--   when (i == "no") $
--     earlyReturn 5
--   if i == "yes"
--      then earlyReturn 2
--      else putStrLn "t"
--   case i of
--     "what" -> earlyReturn 100
--     "huh" -> do
--       putStrLn "yup"
--       earlyReturn 101
--     "nope" -> pure ()
--     _ -> putStrLn "nah"
--   pure 3

k :: Int
k = runIdentity $ do
--   when True (earlyReturn 1)
--   pure (3 :: Int)
  forLoop [2..8] $ \i -> do
    when (i == 5) $ earlyReturn (1 :: Int)
  pure 2
