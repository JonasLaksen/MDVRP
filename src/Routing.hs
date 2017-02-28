module Routing where
import Utils
import Types
import System.Random
import System.Random.Shuffle
import Control.Monad.Random
import Data.List.Split
import Debug.Trace
import Data.List
import Data.Ord
import Evolution
import qualified Scheduling

initPopulation :: Int -> Depot -> [Customer] -> [Route]
initPopulation _ _ [] = []
initPopulation m depot@((d,q),info) customers =
    let (_, newCustomers) = foldr (\c@(_:_:_:_:demand:_) acc@(q,customersInRoute) -> if demand <= q then (q - demand, c:customersInRoute) else acc) (q,[]) customers
    in newCustomers:initPopulation m depot (customers \\ newCustomers)

fitness :: (RandomGen g) => Int -> Depot -> [Route] -> Rand g Float
fitness m depot routes = do
    evoRoutes <- mapM (\ route -> apply 3 50 0.5 4 (Scheduling.fitness depot) Scheduling.crossover Scheduling.mutate (take 20 $ repeat route)) $ shrink routes
    let shortests = map (maximumBy (comparing $ Scheduling.fitness' depot)) evoRoutes
        totalDistance = sum $ map (routeDistance depot) shortests
        demands = foldr (*) 1.0 $ map (\route -> if coverDemands depot route then 1.0 else 0.1) routes
        routeConstraint = fromIntegral $ boolToInt (m >= length routes)
    return (routeConstraint * demands * (1 / totalDistance))

crossover :: (RandomGen g) => [Route] -> [Route] -> Rand g [[Route]]
crossover r1s r2s =
    do
        rs <- getRandoms
        ss <- getRandoms
        let (ms1,ms2) = (mostSimilar r1s r2s, mostSimilar r2s r1s)
            (diff1, diff2) = (ms1 \\ ms2, ms2 \\ ms1)
            (sol1, sol2) = ((delete' diff2 (r1s \\ [ms1])) ++ [ms2],(delete' diff1 (r2s \\ [ms2])) ++ [ms1])
            (asol1, asol2) = (foldr (\(x,r) acc -> insertInto x ((r::Int) `mod` (length $ concat acc)) acc) sol1 $ zip diff1 rs, foldr (\(x,r) acc -> insertInto x ((r::Int) `mod` (length $ concat acc)) acc) sol2 $ zip diff2 ss)
        return [asol1, asol2]

delete' :: (Eq a) => [a] -> [[a]] -> [[a]]
delete' [] xxs = xxs
delete' (x:xs) xxs = delete' xs $ Routing.delete x xxs

delete :: (Eq a) => a -> [[a]] -> [[a]]
delete x xxs = map (\xs -> filter (/= x) xs) xxs

mostSimilar :: [Route] -> [Route] -> Route
mostSimilar r1s r2s = maximumBy (comparing (\r1 -> mostSimilar' r1 r2s)) r1s

mostSimilar' :: Route -> [Route] -> Int
mostSimilar' r1 r2s =
    let max = maximumBy (comparing (\r2 -> (length $ intersect r1 r2) `div` (1+((length $ union r1 r2)^2)))) r2s
    in (length $ intersect r1 max) `div` (1 + ((length $ union r1 max)^2))

mutate :: (RandomGen g) => [Route] -> Rand g [Route]
mutate routes =
    do
        rs <- getRandoms
        ss <- getRandoms
        let
            mutated = shrink $ foldr (\(_,(r,s)) routes -> nestedSwap (r `mod` (sum $ map length routes)) (s `mod` (sum $ map length routes)) routes) routes $ zip [1..length routes `div` 2] $ zip rs ss
        return mutated

coverDemands :: Depot -> [Customer] -> Bool
coverDemands ((_,maxLoad),_) customers =
    maxLoad >= sum ( map (\(_:_:_:_:demand:_) -> demand) customers)



