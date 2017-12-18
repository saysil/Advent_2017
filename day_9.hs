
type Garbage = String

data Group = Subgroup [Group] | Garbage Int deriving Show

exitGarbage :: String -> Int -> (String, Int)
exitGarbage ('!':x:xs) n = exitGarbage xs n
exitGarbage ('>':xs) n = (xs, n)
exitGarbage (_:xs) n = exitGarbage xs (n+1)

exitGroup :: String -> String
exitGroup ('}':xs) = xs
exitGroup ('{':xs) = exitGroup (exitGroup xs)
exitGroup ('<':xs) = exitGroup (fst (exitGarbage xs 0))
exitGroup (_:xs) = exitGroup xs

parseSubGroup :: String -> [Group]
parseSubGroup ('<':xs) = (Garbage (snd (exitGarbage xs 0))):(parseSubGroup (fst (exitGarbage xs 0)))
parseSubGroup ('{':xs) = (Subgroup (parseSubGroup xs)):(parseSubGroup (exitGroup xs))
parseSubGroup ('}':xs) = []
parseSubGroup (_:xs) = parseSubGroup xs
parseSubGroup [] = []

score :: Int -> Group -> Int
score _ (Garbage _) = 0
score value (Subgroup []) = value
score value (Subgroup xs) = value + sum (map (score (value+1)) xs)

scoreGarbage :: Int -> Group -> Int
scoreGarbage value (Garbage n) = value + n
scoreGarbage value (Subgroup []) = 0
scoreGarbage value (Subgroup xs) = value + sum (map (scoreGarbage 0) xs)

solve1 :: String -> Int
solve1 str = sum (map (score 1) (parseSubGroup $ head (lines str)))

solve2 :: String -> Int
solve2 str = sum (map (scoreGarbage 0) (parseSubGroup $ head (lines str)))

main = ((readFile "day_9_input.txt") >>= (putStrLn.show.solve2))
--main = (putStrLn.show) (map (score 1) ((parseSubGroup "{{<a!>}, {<a!>}, {<a!>}, {<ab>}}")))

