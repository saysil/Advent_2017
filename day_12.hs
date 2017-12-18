
import Data.Char
import Data.HashMap

type PipeID = Int
type Pipe = [PipeID]

type PipeList = Map PipeID Pipe
type PipesSeen = Map PipeID Bool

zeroHelper :: PipeList -> (Int, PipesSeen) -> PipeID -> (Int, PipesSeen)
zeroHelper list (_, seen) id = countZeroes list id seen

countZeroes :: PipeList -> PipeID -> PipesSeen -> (Int, PipesSeen)
countZeroes list x seen
        | member x seen = (0, seen)
        | otherwise = (1+(sum (Prelude.map fst new)), (snd.last) new)
        where new = (scanl (zeroHelper list)) (0, insert x True seen) (list!x)

parseTree :: [[String]] -> PipeList
parseTree [] = empty
parseTree ((f:_:ls):xs) = union (singleton (read f) (Prelude.map (read.(Prelude.filter isAlphaNum)) ls)) (parseTree xs)

parseWhole :: String -> PipeList
parseWhole str = parseTree (Prelude.map words (lines str))

countGroups :: PipeID -> PipeList -> Int -> Int
--countGroups _ empty score = score
countGroups id list score
        | Data.HashMap.null list = score
        | otherwise = countGroups (head (keys newList)) newList (1+score)
        where newList = difference list (snd (countZeroes list id empty))

solve1 :: String -> Int
solve1 str = fst $ countZeroes (parseWhole str) 0 empty

solve2 :: String -> Int
solve2 str = countGroups 0 (parseWhole str) 0

main = ((readFile "day_12_input.txt") >>= (putStrLn.show.solve2))
--main = (putStrLn.show) (solve2 "0 <-> 2\n1 <-> 1\n2 <-> 0 3 4\n3 <-> 2 4\n4 <-> 2 3 6\n5 <-> 6\n6 <-> 4 5")

