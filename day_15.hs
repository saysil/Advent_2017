
import Data.Int

type LCGState = Int
type LCGFactor = Int
type LCGMod = Int

type LCG = (LCGState, LCGFactor, LCGMod)

stepLCG :: LCG -> LCG
stepLCG ((a, b, n)) = (((a*b) `mod` n), b, n)

st :: LCG -> Int
st (a, _, _) = a

step :: Int -> Int -> LCG -> LCG -> Int
step 40000000 score _ _ = score
step n score lcg1 lcg2
        | ((fromIntegral (st lcg1))::Int16) == ((fromIntegral (st lcg2))::Int16) = step (n+1) (score+1) new1 new2
        | otherwise = step (n+1) (score) new1 new2
        where (new1, new2) = (stepLCG lcg1, stepLCG lcg2)      

main = (putStrLn.show) (step 0 0 ((1, 16807, maxBound)) ((1, 8921, maxBound)))

