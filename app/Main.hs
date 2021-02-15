module Main where

import Lib ( someFunc )
import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe
import Compare ( makeTSCompare )

file1 :: [(Int, Double )]
file1 = [ (1, 200.1), (2, 199.5), (3, 199.4)
        , (4, 198.1), (5, 199.0), (6, 200.2)
        , (9, 200.3), (20, 201.2), (12, 202.9) ]

file2 :: [(Int, Double)]
file2 = [ (11, 201.1), (12, 199.5), (13, 199.4)
        , (14, 197.1), (15, 199.0), (16, 200.2)
        , (18, 201.3), (20, 201.2) ]

file3 :: [(Int, Double)]
file3 = [ (10, 200.1), (11, 199.5), (12, 199.4)
        , (13, 198.1), (14, 199.0), (17, 200.2)
        , (24, 200.3), (25, 201.2) ]

file4 :: [(Int, Double)]
file4 = [ (26, 197.1), (27, 199.5), (28, 199.4)
        , (29, 198.1), (30, 198.0), (31, 200.2)
        , (32, 200.3), (33, 203.2), (34, 202.9) 
        , (35, 200.3), (36, 200.9) ]

data TS a = TS [Int] [Maybe a]

createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
    where completeTimes = [minimum times .. maximum times]
          timeValueMap = Map.fromList (zip times values)
          extendedValues = map (\v -> Map.lookup v timeValueMap) completeTimes

fileToTS :: [(Int, a)] -> TS a
fileToTS tvPairs = createTS times values
    where (times, values) = unzip tvPairs

showTVPair :: Show a => Int -> Maybe a -> String 
showTVPair time (Just value) = mconcat [show time, "|", show value, "\n"]
showTVPair time Nothing  = mconcat [show time, "|NA", "\n"]

instance Show a => Show (TS a) where
    show (TS times values) = mconcat rows
        where rows = zipWith showTVPair times values


ts1 :: TS Double 
ts1 = fileToTS file1

ts2 :: TS Double 
ts2 = fileToTS file2

ts3 :: TS Double 
ts3 = fileToTS file3

ts4 :: TS Double 
ts4 = fileToTS file4

insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing ) = myMap
insertMaybePair myMap (k, Just value ) = Map.insert k value myMap

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
    where bothTimes = mconcat [t1, t2]
          completeTimes = [minimum bothTimes .. maximum bothTimes]
          tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
          updatedMap = foldl insertMaybePair tvMap (zip t2 v2)
          combinedValues = map (\v -> Map.lookup v updatedMap) completeTimes

instance Semigroup (TS a) where
    (<>) = combineTS

instance Monoid (TS a) where
    mempty = TS [] []
    mappend = (<>)

tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

mean :: (Real a) => [a] -> Double 
mean xs = total/count
    where total = (realToFrac . sum) xs
          count = (realToFrac . length) xs

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS times values) = if all (== Nothing ) values
                           then Nothing 
                           else Just avg
    where justVals = filter isJust values
          cleanVals = map fromJust justVals
          avg = mean cleanVals   

compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS func (TS [] [] ) = Nothing
compareTS func (TS times values) = if all (== Nothing) values
                                   then Nothing 
                                   else Just best
    where pairs = zip times values
          best = foldl (makeTSCompare func) (0, Nothing) pairs

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

main :: IO ()
main = someFunc
