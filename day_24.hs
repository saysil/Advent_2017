
import Data.List
import Debug.Trace
import Data.Maybe

data Part = Part Int Int deriving (Eq, Show)

reversePart :: Part -> Part
reversePart (Part a b) = (Part b a)

matchesPart :: Part -> Part -> Maybe Part
matchesPart (Part a1 b1) p@(Part a2 b2)
        | b1 == a2 = Just p
        | b1 == b2 = Just (reversePart p)
        | otherwise = Nothing

nextParts :: [Part] -> Part -> [Part]
nextParts xs p = mapMaybe (matchesPart p) xs

hasZero :: Part -> Maybe Part
hasZero a@(Part 0 _) = Just a
hasZero a@(Part _ 0) = Just (reversePart a)
hasZero _ = Nothing

addNextParts :: ([Part], [Part]) -> [([Part], [Part])]
addNextParts (xs, list) = map (\x->(x:xs, delete (reversePart x) (delete x list))) possible
        where possible = nextParts list (head xs)

addParts :: [([Part], [Part])] -> [[Part]]
addParts [] = []
addParts xs = (map fst xs) ++ (addParts newbridges)
        where newbridges = xs >>= addNextParts

getZeroParts :: [Part] -> [([Part], [Part])]
getZeroParts xs = map (\x->([x], delete (reversePart x) (delete x xs))) zeroParts
        where zeroParts = mapMaybe hasZero xs

constructBridges :: [Part] -> [[Part]]
constructBridges = (addParts.getZeroParts)

replaceSlash :: String -> String
replaceSlash [] = []
replaceSlash (x:xs) = (if x == '/' then ' ' else x):(replaceSlash xs)

parseLine :: [[String]] -> [Part]
parseLine [] = []
parseLine ((a:b:[]):xs) = (Part (read a) (read b)):(parseLine xs)

countStrength :: [Part] -> Int
countStrength [] = 0
countStrength ((Part a b):xs) = a + b + (countStrength xs)

solve1 :: String -> Int
solve1 str = (maximum.(map countStrength)) ((constructBridges.parseLine) (map words (lines (replaceSlash str))))

takeFromSorted :: [[a]] -> Int -> [[a]]
takeFromSorted (x:xs) b
        | length x < b = []
        | otherwise = x:(takeFromSorted xs b)

solve2 :: String -> Int
solve2 str = (maximum.(map countStrength)) $ takeFromSorted sorted (length (head sorted))
        where bridges = ((constructBridges.parseLine) (map words (lines (replaceSlash str))))
              sorted = (sortBy (\y->(\x->compare (length x) (length y))) bridges)

main = ((readFile "day_24_input.txt") >>= (putStrLn.show.solve2))

