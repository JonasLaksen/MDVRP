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
import Scheduling

main =  do
    content <- readFile "data/problems/p05"
    let lines =  splitOn "\n" $ Data.List.Utils.replace "\r" "" content
        matrix =  map (\x -> map (\x -> read x :: Int ) . take 5 . filter (\x -> length x > 0) $ splitOn " " x) lines
        (m:n:t:_) = head matrix
        depotInfo= take t $ drop 1 matrix
        customers = take n $ drop (t+1) matrix
        depots = zipWith (\a (b:c:_) -> ((b,c), a)) (take t $ drop (1 + n + t) matrix) $ take t $ drop 1 matrix

        g = grouping depots customers
        (depot:_) = keys g
        (group:_) = elems g

    let initialPopulation = take 100 $ repeat $ initPopulation m depot group
--     population <- run 1000 100 100 m 0.5 4 depot initialPopulation

    let best = head $ maximumBy (comparing $ Routing.fitness m depot) initialPopulation
        distance = routeDistance depot best


    toFile def ( "images/maplol.svg") $
        do
            layout_title .= "MDVRP - Distance " ++ show distance
            plot (line "Group" $ [routeCycle depot best])
            plot (points "Depots" [position $ snd depot])
--         lines = map (\route -> routeCycle depot route) best

    putStrLn $ show $ distance
    rt <- evalRandIO $ apply 10 30 0.4 4 (Scheduling.fitness depot) Scheduling.crossover Scheduling.mutate (take 10 $ repeat best)
    let shortest = maximumBy (comparing $ Scheduling.fitness depot) rt
    let d2 = routeDistance depot shortest
    putStrLn $ show d2

    toFile def ( "images/map.svg") $
        do
            layout_title .= "MDVRP - Distance " ++ show d2
            plot (line "Group" $ [routeCycle depot shortest])
            plot (points "Depots" [position $ snd depot])

    return "lol"

run :: Int -> Int -> Int -> Int -> Float -> Int -> Depot -> [[Route]] -> IO ([[Route]])
run it it2 populationSize m survivalRate fertility depot population
    | it <= 0 = return population
    | otherwise = do
        finalPopulation <- evalRandIO $ apply it2 populationSize survivalRate fertility (Routing.fitness m depot) Routing.crossover Routing.mutate population
        let best = maximumBy (comparing $ Routing.fitness m depot) finalPopulation
            distance = sum $ map (routeDistance depot) best
            lines = map (\route -> routeCycle depot route) best
        putStrLn $ show $ prettySolution best

        toFile def ( "images/map" ++ show (it - it2) ++ ".svg") $
            do
                layout_title .= "MDVRP - Distance " ++ show distance
                mapM_ (\x -> plot (line "Group" [x])) $ lines
                plot (points "Depots" [position $ snd depot])
        run (it - it2) it2 populationSize m survivalRate fertility depot finalPopulation