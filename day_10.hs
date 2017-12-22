
{-# LANGUAGE ViewPatterns #-}

import Data.Sequence as S
import Data.Foldable
import Data.Char
import Data.Bits

import Numeric

type Position = Int
type Length = Int

defaultList = S.fromList [0, 1..255]

modTake :: Seq Int -> Position -> Int -> Seq Int
modTake ls pos 0 = empty
modTake ls pos n = (index ls curPos)<|(modTake ls (curPos+1) (n-1))
        where curPos = pos `mod` (S.length ls)

modReplace :: Seq Int -> Position -> Seq Int -> Seq Int
modReplace (S.viewl -> S.EmptyL) _ seq = seq
modReplace (S.viewl -> (a:<seq)) pos newSeq = modReplace seq (curPos+1) (adjust (const a) (curPos) newSeq)
        where curPos = pos `mod` (S.length newSeq)


step :: Seq Int -> Position -> Length -> Seq Int
step seq pos len = modReplace (S.reverse (modTake seq pos len)) pos seq

type ListInfo = (Position, Seq Int, Int)

helperStep :: ListInfo -> Length -> ListInfo
helperStep (pos, seq, skip) len = (pos+skip+len, step seq pos len, skip+1)

getSeq :: ListInfo -> Seq Int
getSeq (_, seq, _) = seq

run :: Seq Int -> [Length] -> Seq Int
run init ls = getSeq (Prelude.foldl (helperStep) (0, init, 0) ls)

solve1 :: String -> Int
solve1 str = (index ls 0)*(index ls 1)
        where ls = (run defaultList (map read (words str)))


standardSuffix = [17, 31, 73, 47, 23]

stringToLengths :: String -> [Length]
stringToLengths str = (map ord str) ++ standardSuffix

xor16 :: [Int] -> Int
xor16 ls = foldl xor 0 (ls)

finishHash :: [Int] -> [Int]
finishHash [] = []
finishHash ls = (xor16 (Prelude.take 16 ls)):(finishHash (Prelude.drop 16 ls))

toHexHelper :: Int -> String
toHexHelper n = if n < 16 then ('0':(showHex n "")) else (showHex n "")

toHex :: [Int] -> String
toHex ls = "0x" ++ concat (map toHexHelper ls)

toBytes :: String -> String
toBytes str = map chr (map read (words $ map (\x->(if x==',' then ' ' else x)) str))

solve2 :: String -> String
solve2 str = (toHex.finishHash) (toList ls)
        where ls = (run defaultList (concat (Prelude.replicate 64 (stringToLengths str))))

--main = ((readFile "day_10_input.txt") >>= (putStrLn.show.stringToLengths))
main = (putStrLn.show) ((solve2) "130,126,1,11,140,2,255,207,18,254,246,164,29,104,0,224")
--main = ((readFile "day_10_input.txt") >>= (putStrLn.show.solve2))

