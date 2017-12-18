
import Data.Sequence as DSeq

data DanceMove = Spin Int | Exchange Int Int | Partner Char Char deriving Show

type Dancers = Seq Char

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

solve1 :: String -> Dancers
solve1 str = (dance (parseDance (lines str)) defaultDancers)

main = (putStrLn.show) ("test")

