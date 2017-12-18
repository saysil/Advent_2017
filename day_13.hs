
import Data.Char

position :: Int -> Int -> Int
position length time = -(abs ((time `mod` (2*(length-1))) - (length-1))) + (length-1)

caught :: (Int, Int) -> Int -> Bool
caught (depth, length) time = (position length time) == 0

value :: (Int, Int) -> Int
value (depth, length) = depth*length

run :: Int -> [(Int, Int)] -> Int
run time (x:[])
        | caught x time = value x
        | otherwise = 0
run time (x@(depth, length):xs)
        | caught x time = (value x) + run (time + (fst (head xs) - depth)) xs
        | otherwise = run (time + (fst (head xs) - depth)) xs

shortCircuitRun :: Int -> [(Int, Int)] -> Bool
shortCircuitRun time (x:[])
        | caught x time = False
        | otherwise = True
shortCircuitRun time (x@(depth, length):xs)
        | caught x time = False
        | otherwise = shortCircuitRun (time + (fst (head xs) - depth)) xs

parseLn :: [String] -> (Int, Int)
parseLn (depth:length:[]) = (read depth, read length)
parseLn _ = error "No parse avaialable"

parse :: String -> [(Int, Int)]
parse str = (map (parseLn.words.(filter (not.isPunctuation))) (lines str))

solve1 :: String -> Int
solve1 = (run 0).parse


checkSolve2 :: [(Int, Int)] -> Int -> Int
checkSolve2 xs a 
        | shortCircuitRun a xs = a
        | otherwise = checkSolve2 xs (a+1)

solve2 :: String -> Int
solve2 str = checkSolve2 (parse str) 1

main = ((readFile "day_13_input.txt") >>= (putStrLn.show.solve2))
--main = (putStrLn.show) (solve2 "0: 3\n1: 2\n4: 4\n6: 4")

