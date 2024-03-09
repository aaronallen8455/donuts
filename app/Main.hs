{-# OPTIONS_GHC -fplugin Donuts.Plugin #-}
module Main where

import           Control.Monad (forever, void)
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Data.Maybe
import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map.Strict as M
import           Data.Functor.Identity
import           Donuts

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
    go oldR 0 oldS s oldT t = (oldS, oldT, oldR, t, s)
    go !oldR !r !oldS !s !oldT !t =
      let quotient = div oldR r
       in go r (oldR - quotient * r)
             s (oldS - quotient * s)
             t (oldT - quotient * t)

extendedGcd'' :: Int -> Int -> (Int, Int, Int, Int, Int)
extendedGcd'' a b = (`evalState` (a, b)) . (`evalStateT` (1, 0)) . (`evalStateT` (0, 1)) $ do
  void . runExceptT . forever $ do
    rTup <- lift . lift $ lift get
    when (snd rTup == 0) (throwE ())
    let quotient = uncurry div rTup
    lift . lift . lift $ put (snd rTup, fst rTup - quotient * snd rTup)
    sTup <- lift $ lift get
    lift . lift $ put (snd sTup, fst sTup - quotient * snd sTup)
    tTup <- lift get
    lift $ put (snd tTup, fst tTup - quotient * snd tTup)

  rTup <- lift $ lift get
  sTup <- lift get
  tTup <- get

  pure (fst sTup, fst tTup, fst rTup, snd tTup, snd sTup)


bfs :: Graph -> String -> String -> Graph
bfs g start end = runIdentity $ do
  let Mut res = g
  let Mut queue = [start]
  whileL (not $ null queue) $ do
    let Mut newQueue = []
    forL queue $ \vIdx -> do
      when (vIdx == end) $ earlyReturn res
      let es = maybe [] edges $ M.lookup vIdx res
      forL es $ \i -> do
        let node = res M.! i
        when (isJust (parent node) || i == start) continueL
        res := M.insert i (node {parent = Just vIdx}) res
        newQueue := i : newQueue
    queue := newQueue
  pure res

bfs' :: Graph -> String -> String -> Graph
bfs' graph start end = go graph [start] []
  where
    go g [] [] = g
    go g [] newQueue = go g newQueue []
    go g (vIdx : queue) newQueue
      | vIdx == end = g
      | otherwise =
        let es = maybe [] edges $ M.lookup vIdx g
            (newG, newerQueue) = foldl' (go2 vIdx) (g, newQueue) es
         in go newG queue newerQueue

    go2 r (!g, queue) e =
      let node = g M.! e
       in if isJust (parent node) || e == start
             then (g, queue)
             else ( M.insert e node { parent = Just r } g
                  , e : queue
                  )

-- floydWarshall :: Graph -> Map [String] Int
-- floydWarshall graph = runIdentity $ do
--   let labels = V.fromList $ M.keys graph
--       sz = V.length labels
--   let Mut result = M.empty
-- 
--   forL (M.toList graph) $ \(l, es) ->
--     forL (edges es) $ \e -> do
--       result := M.insert (sort [l, e]) 1 result
-- 
--   forL labels $ \j ->
--     forL [0 .. sz - 1] $ \ix -> do
--       let i = labels V.! ix
--       forL [ix + 1 .. sz - 1] $ \kx -> do
--         let k = labels V.! kx
--         when (i == j || k == j) continueL
--         let direct = M.findWithDefault maxBound (sort [i, k]) result
--             withJ = fromMaybe maxBound
--                   $ (+) <$> M.lookup (sort [i, j]) result
--                         <*> M.lookup (sort [j, k]) result
--         when (withJ < direct) $ do
--           result := M.insert (sort [i, k]) withJ result
-- 
--   pure result





















main :: IO ()
main = do
--   g <- parseGraph <$> getContents
--   let res = bfs g "qcc" "xkx"
--   print $ extractPath res "xkx"
  pure ()

parseGraph :: String -> Graph
parseGraph =
  fmap mkNode . M.fromListWith (<>) . concatMap parseLine . lines

parseLine :: String -> [(String, [String])]
parseLine ln = case splitAt 3 ln of
  (l, _:es) -> (l, words es) : do
    e <- words es
    [(e, [l])]
  _ -> undefined

mkNode :: [String] -> Node
mkNode es = Node es Nothing

extractPath :: Graph -> String -> [String]
extractPath g l =
  case M.lookup l g of
    Nothing -> undefined
    Just n ->
      l : foldMap (extractPath g) (parent n)

data Node =
  Node
    { edges :: ![String]
    , parent :: !(Maybe String)
    } deriving Show

type Graph = Map String Node

