
import Data.Char

-- Task: Sum the same digits in a repeating string. I.e. 1111 -> 4

getSum :: Int -> [Int] -> Int
getSum 0 _ = 0
getSum length (a:(b:bs))
	| a == b = b + getSum (length - 1) (b:bs)
	| otherwise = getSum (length - 1) (b:bs)

sumString :: String -> Int
sumString str = getSum (length str) (cycle (map digitToInt str))

main = (putStrLn.show) (sumString "112232221111")

