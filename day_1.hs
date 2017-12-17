
import Data.Char

-- Task: Sum the same digits in a repeating string. I.e. 1111 -> 4

getSum :: Int -> [Int] -> Int
getSum 0 _ = 0
getSum length (a:(b:bs))
	| a == b = b + getSum (length - 1) (b:bs)
	| otherwise = getSum (length - 1) (b:bs)

sumString :: String -> Int
sumString str = getSum (length str) (cycle (map digitToInt str))

getSum2 :: Int -> Int -> [Int] -> Int
getSum2 0 _ _ = 0
getSum2 remaining skip (x:xs)
        | x == xs!!(skip-1) = x + getSum2 (remaining-1) skip xs
        | otherwise = getSum2 (remaining-1) (skip) xs

solve1 :: String -> Int
solve1 = (sumString.(filter (isAlphaNum)))

solve2 :: String -> Int
solve2 str = (getSum2 (length str) ((length str) `div` 2) (cycle $ map digitToInt (filter (isAlphaNum) str)))

main = ((readFile "day_1_input.txt") >>= (putStrLn.show.solve2))
--main = ((readFile "day_1_input.txt") >>= (putStrLn.show.solve1))

