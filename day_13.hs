
import Data.Char

position :: Int -> Int -> Int
position length time = -(abs ((time `mod` (2*(length-1))) - (length-1))) + (length-1)

caught :: (Int, Int)-> Bool
caught (depth, length) = (position length depth) == 0

value :: (Int, Int) -> Int
value (depth, length) = depth*length

run :: [(Int, Int)] -> Int
run [] = 0
run (x:xs)
        | caught x = (value x) + run xs
        | otherwise = run xs


parseLn :: [String] -> (Int, Int)
parseLn (depth:length:[]) = (read depth, read length)
parseLn _ = error "No parse avaialable"

parse :: String -> [(Int, Int)]
parse str = (map (parseLn.words.(filter (not.isPunctuation))) (lines str))

solve1 :: String -> Int
solve1 = run.parse

main = ((readFile "day_13_input.txt") >>= (putStrLn.show.solve1))
--main = (putStrLn.show) (solve1 "0: 3\n1: 2\n4: 4\n6: 4")

