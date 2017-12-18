
import Data.HashMap as HashMap
import Data.Hashable

import Data.List

verify :: (HashMap.Map String String) -> [String] -> Bool
verify _ [] = True
verify mapping (x:xs)
        | HashMap.member x mapping == True = False
        | HashMap.member x mapping == False = verify (HashMap.insert x x mapping) xs

containsPermutation :: [String] -> [String] -> Bool
containsPermutation permutes [] = False
containsPermutation permutes (x:xs)
        | x `elem` permutes = True
        | otherwise = containsPermutation permutes xs

verify2 :: [String] -> Bool
verify2 [] = True
verify2 (x:xs)
        | containsPermutation (permutations x) xs = False
        | otherwise = verify2 xs

process :: [[String]] -> Int
process arr = sum (Prelude.map (fromEnum.(verify empty)) arr)
-- Convert Trues to 1s and sum

solve1 :: String -> Int
solve1 str = process (Prelude.map words (lines str))

solve2 :: String -> Int
solve2 str = sum (Prelude.map (fromEnum.(\x->(verify empty x) && verify2 x)) (Prelude.map words (lines str)))


main = ((readFile "day_4_input.txt") >>= (putStrLn.show.solve2))
--main = (putStrLn.show) (process [["ee", "aa", "ee"], ["ee", "aa", "eee"]])

