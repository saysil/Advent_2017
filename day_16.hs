
import Data.Sequence as DSeq
import Data.Hashable
import Data.Foldable

data DanceMove = Spin Int | Exchange Int Int | Partner Char Char deriving Show

type Dancers = Seq Char

instance (Hashable a) => Hashable (Seq a) where
        hashWithSalt a list = hashWithSalt a (toList list)

dance :: [DanceMove] -> Dancers -> Dancers
dance [] dancers = dancers
dance ((Spin n):xs) dancers = dance xs (end >< beginning)
        where (beginning, end) = DSeq.splitAt ((DSeq.length dancers) - n) dancers
dance ((Partner a b):xs) dancers = dance xs (update posb a (update posa b dancers))
        where (posa, posb) = (head (elemIndicesL a dancers), head (elemIndicesL b dancers))
dance ((Exchange posa posb):xs) dancers = dance xs (update posb a (update posa b dancers))
        where (a, b) = (index dancers posa, index dancers posb)


defaultDancers = fromList ['a', 'b'.. 'p']

parseExchange :: String -> [Int]
parseExchange xs = map read (words (map (\x->if (x=='/') then ' ' else x) xs))

parseDance :: [String] -> [DanceMove]
parseDance (('s':n):xs) = (Spin (read n)):(parseDance xs)
parseDance (('x':x):xs) = let (a:b:_) = parseExchange x
        in (Exchange a b):(parseDance xs)
parseDance (('p':a:'/':b:[]):xs) = (Partner a b):(parseDance xs)
parseDance [] = []
parseDance (err:_) = error ("No Parse for " ++ err)

replaceCommas :: String -> String
replaceCommas [] = []
replaceCommas (',':xs) = ' ':(replaceCommas xs)
replaceCommas (x:xs) = x:(replaceCommas xs)

parseWhole :: String -> [DanceMove]
parseWhole str = parseDance (words (replaceCommas (head (lines str))))

solve1 :: String -> Dancers
solve1 str = (dance (parseWhole str) defaultDancers)

searchForCycle :: [Int] -> Dancers -> [DanceMove] -> Dancers
searchForCycle hashes dancers moves
        | (hash dancers) `elem` hashes = dancers
        | otherwise = searchForCycle ((hash dancers):hashes) (dance moves dancers) moves

cycleCount :: Dancers -> Dancers -> [DanceMove] -> Int -> Int
cycleCount d copy moves count
        | d == copy = count
        | otherwise = cycleCount (dance moves d) copy moves (count+1)

cycleLength :: [DanceMove] -> Int
cycleLength moves = (cycleCount (dance moves cycle) cycle moves 1)
        where cycle = searchForCycle [] defaultDancers moves


solve2 :: [DanceMove] -> Dancers
solve2 moves = last (Prelude.take (((1000000000 - (cycleCount defaultDancers cycle moves 0)) `mod` (cycleLength moves))+1) (iterate (dance moves) cycle))
        where cycle = searchForCycle [] defaultDancers moves


main = ((readFile "day_16_input.txt") >>= (putStrLn.show.solve2.parseWhole))

