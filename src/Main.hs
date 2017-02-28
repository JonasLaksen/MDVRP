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
    content <- readFile "data/problems/p04"
    let lines =  splitOn "\n" $ Data.List.Utils.replace "\r" "" content
        matrix =  map (\x -> map (\x -> read x :: Int ) . take 5 . filter (\x -> length x > 0) $ splitOn " " x) lines
        (m:n:t:_) = head matrix
        depotInfo= take t $ drop 1 matrix
        customers = take n $ drop (t+1) matrix
        depots = zipWith (\a (b:c:_) -> ((b,c), a)) (take t $ drop (1 + n + t) matrix) $ take t $ drop 1 matrix

        g = grouping depots customers
        ds = keys g
        gs = elems g
    hahah <- mapM (\(depot,group) -> lolz m depot group) $ zip ds gs
    createPlot $ map (\(d,rs) -> (map (\r -> routeCycle (d::Depot) (r::Route)) rs)) (hahah::[(Depot, [Route])] )
    return "lol"

lolz :: Int -> Depot -> [Customer] -> IO (Depot,[Route])
lolz m depot group = do
    initialPopulation <- evalRandIO $ mapM Routing.mutate $ take 1000 $ repeat $ initPopulation m depot group
    population <- run 200 10 150 m 0.2 4 depot initialPopulation
    fitnesses <- evalRandIO $ mapM (Routing.fitness m depot) population
    let best = fst $ maximumBy (comparing snd) $ zip population fitnesses
    sols <- mapM (\sol -> evalRandIO $ apply 100 100 0.5 4 (Scheduling.fitness depot) Scheduling.crossover Scheduling.mutate (take 50 $ repeat sol)) best
    let loll = map (maximumBy (comparing (Scheduling.fitness' depot))) sols
    return (depot,loll)

run :: Int -> Int -> Int -> Int -> Float -> Int -> Depot -> [[Route]] -> IO ([[Route]])
run it it2 populationSize m survivalRate fertility depot population
    | it <= 0 = return population
    | otherwise = do
        finalPopulation <- trace ("lol") $ evalRandIO $ apply it2 populationSize survivalRate fertility (Routing.fitness m depot) Routing.crossover Routing.mutate population
        fitnesses <- evalRandIO $ mapM (Routing.fitness m depot) finalPopulation
        let best = fst $ maximumBy (comparing snd) $ zip finalPopulation fitnesses
--         createPlot (it-it2) depot best
        run (it - it2) it2 populationSize m survivalRate fertility depot finalPopulation

createPlot :: [[[(Int,Int)]]] -> IO ()
createPlot cycles = do
        toFile def ( "images/map.svg") $
            do
                layout_title .= "MDVRP - Distance "
--                 mapM_ (\x -> plot (line "Group" (x::[[(Int,Int)]]))) cycle
                mapM_ (\c -> plot (line "ok" [c])) $ concat cycles
--                 plot (line "Group" $ concat cycles)
