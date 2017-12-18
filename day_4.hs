
import Data.HashMap as HashMap
import Data.Hashable


verify :: (HashMap.Map String String) -> [String] -> Bool
verify _ [] = True
verify mapping (x:xs)
        | member x mapping == True = False
        | member x mapping == False = verify (insert x x mapping) xs

process :: [[String]] -> Int
process arr = sum (Prelude.map (fromEnum.(verify empty)) arr)
-- Convert Trues to 1s and sum

solve1 :: String -> Int
solve1 str = process (map words (lines str))

main = ((readFile "day_4_input.txt") >>= (putStrLn.show.solve1))
--main = (putStrLn.show) (process [["ee", "aa", "ee"], ["ee", "aa", "eee"]])

