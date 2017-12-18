
import Data.Int

type LCGState = Int64
type LCGFactor = Int64
type LCGMod = Int64

type LCG = (LCGState, LCGFactor, LCGMod)

stepLCG :: LCG -> LCG
stepLCG ((a, b, n)) = ((a*b) `mod` n, b, n)

st :: LCG -> Int64
st (a, _, _) = a

step :: Int -> Int -> LCG -> LCG -> Int
step 40000000 score _ _ = score
step n score lcg1 lcg2
        | ((fromIntegral (st lcg1))::Int16) == ((fromIntegral (st lcg2))::Int16) = step (n+1) (score+1) new1 new2
        | otherwise = step (n+1) (score) new1 new2
        where (new1, new2) = (stepLCG lcg1, stepLCG lcg2)      

getBound = fromIntegral ((maxBound)::Int32)


makeLCGList :: LCG -> [LCG]
makeLCGList = iterate stepLCG

filterLCGA :: [LCG] -> [LCG]
filterLCGA = filter (\x->(st x) `mod` 4 == 0)

filterLCGB :: [LCG] -> [LCG]
filterLCGB = filter (\x->(st x) `mod` 8 == 0)

compare :: LCG -> LCG -> Int
compare lcg1 lcg2
        | ((fromIntegral (st lcg1))::Int16) == ((fromIntegral (st lcg2))::Int16) = 1
        | otherwise = 0

makeA = (873, 16807, getBound)
makeB = (583, 48271, getBound)

solve2 = sum (take 5000000 (zipWith Main.compare (filterLCGA (makeLCGList makeA)) (filterLCGB (makeLCGList makeB))))

main = (putStrLn.show) (solve2)

