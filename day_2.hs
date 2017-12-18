
-- Task: Compute the checksum of a spreadsheet by subtracting the max and the min values for
-- each row and summing them

rowDifference :: [Int] -> Int -> Int -> Int
rowDifference [] min max = (max - min)
rowDifference (x:xs) min max
	| max < x = rowDifference xs min x
	| min > x = rowDifference xs x max
	| otherwise = rowDifference xs min max


rowDiv :: [Int] -> [Int] -> Int
rowDiv [] _ = error "No numbers divisble"
rowDiv (x:xs) copy
        | l == [] = rowDiv xs copy
        | otherwise = if (x `mod` (head l)) == 0 then x `div` (head l) else (head l) `div` x
        where l = filter (\y->(y `mod` x == 0) && (y /= x)) copy

getRow :: [Int] -> Int
getRow (x:xs) = rowDifference (x:xs) x x

checksum :: [[Int]] -> Int
checksum = sum.(map getRow)

parse :: String -> [[Int]]
parse str = map ((map read).words) (lines str)

solve2 :: String -> Int
solve2 str = sum (map (\x->rowDiv x x) (parse str))

main = ((readFile "day_2_input.txt") >>= (putStrLn.show.solve2))
--main = (putStrLn.show) $ checksum (parse "5 1 9 5\n7 5 3\n2 4 6 8")

