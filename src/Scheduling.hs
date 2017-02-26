module Scheduling where

import Types
import Utils
import System.Random
import Control.Monad.Random
import Data.List

fitness :: Depot -> Route -> Float
fitness depot route = 1.0 / routeDistance depot route

crossover :: (RandomGen g) => Route -> Route -> Rand g [Route]
crossover [] _ = return []
crossover x y = do
    (r:_) <- getRandoms
    let index = r `mod` length x
        sol1 = normalize (y \\ (take index x ++ drop index y)) (take index x ++ drop index y)
        sol2 = normalize (x \\ (take index y ++ drop index x)) (take index y ++ drop index x)
    return [sol1, sol2]

normalize :: Route -> Route -> Route
normalize all route =
    let counted = foldr (\(i,c) acc -> if c `elem` take i route then ((c,2):acc) else ((c,1):acc)) [] $ zip [0..] route
        replaced = snd $ foldr (\(c, count) (all, cs) -> if count == 1 then (all, c:cs) else (drop 1 all, head all:cs)) (all,[]) counted
    in replaced

mutate :: (RandomGen g) => Route -> Rand g Route
mutate route = do
    (s:_) <- getRandoms
    (t:_) <- getRandoms
    return $ swapElementsAt (s `mod` length route) (t `mod` length route) route
