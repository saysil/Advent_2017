
import System.Posix.Unistd
import Data.Maybe
import Data.Hashable
import Data.HashMap as HashMap
import Data.Sequence as Sequence

data Memory = Memory [Int] (Seq Int) Int

instance Eq Memory where
        (Memory _ xsa _) == (Memory _ xsb _) = xsa == xsb
        (Memory _ xsa _) /= (Memory _ xsb _) = xsa /= xsb

instance Show Memory where
        show (Memory _ xs _) = show xs

instance Hashable Memory where
        hashWithSalt _ = (hash.show)

instance Ord Memory where
        compare a b = compare (hash a) (hash b)

checkHash :: Memory -> Bool
checkHash (mem@(Memory tree _ _)) = elem (hash mem) tree

distribute :: Int -> Memory -> Memory
distribute 0 m = m
distribute (value) (Memory tree xs n) = distribute (value-1) newMem
        where newMem = (Memory tree (Sequence.adjust (+1) n xs) ((n+1) `mod` (Sequence.length xs)))


run :: Memory -> Int -> Int
run mem@(Memory tree xs n) a 
        | (not.checkHash) (mem) = run (distribute (maximum xs) (Memory ((hash mem):tree) newxs ((maxindex+1) `mod` (Sequence.length xs)))) (a+1)
        | otherwise = a
        where (maxindex, newxs) = (fromJust (elemIndexL (maximum xs) xs), Sequence.update maxindex 0 xs)


solve1 :: String -> Int
solve1 str = run (Memory [] (Sequence.fromList ((Prelude.map read) (words (head (lines str))))) 0) 0

main = ((readFile "day_6_input.txt") >>= (putStrLn.show.solve1))
--main = (putStrLn.show) (run (Memory [] (Sequence.fromList [0, 2, 7, 0]) 0) 0)
--main = (putStrLn.show) $ hash (Memory [] (Sequence.fromList [0, 2, 7, 0]) 1)

