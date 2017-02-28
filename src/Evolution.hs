module Evolution where
import System.Random
import System.Random.Shuffle
import Control.Monad.Random
import Data.List.Split
import Data.Ord
import Data.List
import Utils
import Debug.Trace
import Types


apply :: (RandomGen g, Show a) => Int -> Int -> Float -> Int -> (a -> Rand g Float) -> (a -> a -> Rand g [a]) -> (a -> Rand g a) -> [a] -> Rand g [a]
apply 0 _ _ _ _ _ _ solutions = return solutions
apply it populationSize survivalRate fertility fitnessF crossoverF mutationF solutions =
    do
        parents <- select (round $ fromIntegral populationSize * survivalRate) fitnessF solutions
        shuffledParents <- shuffleM parents
        let (mothers:fathers:_) = if length parents < 2
                                    then ([head parents]:[head parents]:[])
                                    else chunksOf (length parents `div` 2) shuffledParents
        children <- mapM (\(m,f) -> mapM (\_ -> crossoverF m f) [1..fertility]) $ zip mothers fathers
        mutatedChildren <- mapM mutationF $ concat $ concat children
        let surviving = mutatedChildren ++ parents
        apply (it - 1) populationSize survivalRate fertility fitnessF crossoverF mutationF surviving

select :: (RandomGen g) => Int -> (a -> Rand g Float) -> [a] -> Rand g [a]
select n fitnessF solutions =
    do
        rs <- getRandomRs(0.0, 1.0)
        evaluatedSolutions <- mapM fitnessF solutions
        let
            sorted =  map snd . reverse . sortBy (comparing fst) $ zip evaluatedSolutions solutions
            zipped = zip sorted $ zip rs [1..length sorted]
            survived = map fst $ filter (\(solution, (r, i)) -> (r::Float) <= (exp $ negate (fromIntegral i / (fromIntegral $ length sorted)))) zipped
        return $ take n survived
