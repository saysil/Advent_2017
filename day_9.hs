
type Garbage = String

data Group = Subgroup [Group] | Garbage deriving Show

exitGarbage :: String -> String
exitGarbage ('!':x:xs) = exitGarbage xs
exitGarbage ('>':xs) = xs
exitGarbage (_:xs) = exitGarbage xs

exitGroup :: String -> String
exitGroup ('}':xs) = xs
exitGroup ('{':xs) = exitGroup (exitGroup xs)
exitGroup ('<':xs) = exitGroup (exitGarbage xs)
exitGroup (_:xs) = exitGroup xs

parseSubGroup :: String -> [Group]
parseSubGroup ('<':xs) = Garbage:(parseSubGroup (exitGarbage xs))
parseSubGroup ('{':xs) = (Subgroup (parseSubGroup xs)):(parseSubGroup (exitGroup xs))
parseSubGroup ('}':xs) = []
parseSubGroup (_:xs) = parseSubGroup xs
parseSubGroup [] = []

score :: Int -> Group -> Int
score _ Garbage = 0
score value (Subgroup []) = value
score value (Subgroup xs) = value + sum (map (score (value+1)) xs)

solve1 :: String -> Int
solve1 str = sum (map (score 1) (parseSubGroup $ head (lines str)))

main = ((readFile "day_9_input.txt") >>= (putStrLn.show.solve1))
--main = (putStrLn.show) (map (score 1) ((parseSubGroup "{{<a!>}, {<a!>}, {<a!>}, {<ab>}}")))

