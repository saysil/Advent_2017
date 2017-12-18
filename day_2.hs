
-- Task: Compute the checksum of a spreadsheet by subtracting the max and the min values for
-- each row and summing them

rowDifference :: [Int] -> Int -> Int -> Int
rowDifference [] min max = (max - min)
rowDifference (x:xs) min max
	| max < x = rowDifference xs min x
	| min > x = rowDifference xs x max
	| otherwise = rowDifference xs min max

getRow :: [Int] -> Int
getRow (x:xs) = rowDifference (x:xs) x x

checksum :: [[Int]] -> Int
checksum = sum.(map getRow)

parse :: String -> [[Int]]
parse str = map ((map read).words) (lines str)

main = ((readFile "day_2_input.txt") >>= (PutStrLn.show.checksum.parse))
--main = (putStrLn.show) $ checksum (parse "5 1 9 5\n7 5 3\n2 4 6 8")

