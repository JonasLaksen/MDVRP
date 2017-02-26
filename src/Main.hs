module Main where
import Data.List
import Data.List.Split
import Data.List.Utils
import Data.Ord
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Utils
import Grouping
import Routing
import Data.Map (fromListWith, elems, keys)
import Debug.Trace
import System.Random
import System.CPUTime
import Control.Monad.Random
import Evolution
import Types

main =  do
    content <- readFile "data/problems/p05"
    let lines =  splitOn "\n" $ replace "\r" "" content
        matrix =  map (\x -> map (\x -> read x :: Int ) . take 5 . filter (\x -> length x > 0) $ splitOn " " x) lines
        (m:n:t:_) = head matrix
        depotInfo= take t $ drop 1 matrix
        customers = take n $ drop (t+1) matrix
        depots = zipWith (\a (b:c:_) -> ((b,c), a)) (take t $ drop (1 + n + t) matrix) $ take t $ drop 1 matrix

        g = grouping depots customers
        (_:depot:_) = keys g
        (_:group:_) = elems g

--     initialPopulation <- evalRandIO $ mapM mutate $ take 100 $ repeat $ initPopulation m depot group
    let initialPopulation = take 100 $ repeat $ initPopulation m depot group
    putStrLn $ show $ prettySolution $ head initialPopulation
    population <- run 1000 100 100 m 0.5 4 depot initialPopulation
    putStrLn $ show $ length population
--     let lol = [1,2,3,4]
--         swapped = swap 3 lol
--     putStrLn $ show swapped
    return "lol"

run :: Int -> Int -> Int -> Int -> Float -> Int -> Depot -> [[Route]] -> IO ([[Route]])
run it it2 populationSize m survivalRate fertility depot population
    | it <= 0 = return population
    | otherwise = do
        finalPopulation <- evalRandIO $ apply it2 populationSize survivalRate fertility (fitness m depot) crossover mutate population
        let best = maximumBy (comparing $ fitness m depot) finalPopulation
            distance = sum $ map (routeDistance depot) best
            lines = map (\route -> routeCycle depot route) best
        putStrLn $ show $ prettySolution best

        toFile def ( "images/map" ++ show (it - it2) ++ ".svg") $
            do
                layout_title .= "MDVRP - Distance " ++ show distance
                mapM_ (\x -> plot (line "Group" [x])) $ lines
                plot (points "Depots" [position $ snd depot])
        run (it - it2) it2 populationSize m survivalRate fertility depot finalPopulation