module Grouping where
import Utils
import Data.List
import Data.Ord
import Data.Map (Map, fromListWith)
import Types

grouping :: [Depot] -> [Customer] -> Map Depot [Customer]
grouping d c = fromListWith (++) $ map (\(a,b) -> (a, [b])) $ grouping' d c

grouping' :: [Depot] -> [Customer] -> [(Depot, Customer)]
grouping' _ [] = []
grouping' depots (c:customers) =
            (minimumBy (comparing (\d -> distance (position $ snd d) (position c))) depots, c) : grouping' depots customers

