
{-# LANGUAGE ViewPatterns #-}

import Data.Sequence as S

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
        where ls = (run defaultList (map read (lines str)))


main = ((readFile "day_10_input.txt") >>= (putStrLn.show.solve1))

