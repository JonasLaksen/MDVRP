module Utils where
import Debug.Trace
import Types

distance :: Point -> Point -> Float
distance (a1,a2) (b1,b2) = sqrt $ fromIntegral $ (a1 - b1) ^ 2 + (a2 - b2) ^ 2

totalDistance :: [Point] -> Float
totalDistance ccs@(c:circle) =
    snd $ foldr (\x acc -> (x, snd acc + distance x (fst acc))) (c, 0) ccs

position :: Entity -> Point
position (_:x:y:_) = (x,y)

prettyCustomer :: Customer -> Int
prettyCustomer (id:_) = id

prettyRoute :: (Route -> [Int])
prettyRoute = map prettyCustomer

prettySolution :: [Route] -> [[Int]]
prettySolution = map prettyRoute

insertInto :: a -> Int -> [[a]] -> [[a]]
insertInto _ _ [] = []
insertInto a i (x:xs)
    | i <= length x = ((insertInto' a i x):xs)
    | otherwise    = x : insertInto a (i - length x) xs

insertInto' :: a -> Int -> [a] -> [a]
insertInto' _ (-1) xxs = xxs
insertInto' a 0 xxs = (a:xxs)
insertInto' _ _ [] = []
insertInto' a i (x:xs) = x : insertInto' a (i-1) xs

shrink :: [[a]] -> [[a]]
shrink = filter (\x -> length x > 0)

swap :: Int -> [a] -> [a]
swap _ []  = []
swap 0 (x:y:ys) = (y:x:ys)
swap i (x:xs) = x : swap (i-1) xs

swapElementsAt i j ls = [get k x | (k, x) <- zip [0..length ls - 1] ls]
    where get k x | k == i = ls !! j
                  | k == j = ls !! i
                  | otherwise = x
