
import Data.List
import Data.Ratio
import Data.Char

type Vec = (Integer, Integer, Integer)

type Particle = (Int, Vec, Vec, Vec)

kinematic :: Integer -> Integer -> Integer -> Integer -> Integer
kinematic a v x t = x + v*t + round (fromRational ((a % 2)*(fromIntegral (t^2))))

timeFunc :: Vec -> Vec -> Vec -> Integer -> Vec
timeFunc (ax, ay, az) (vx, vy, vz) (x, y, z) t = (kinematic ax vx x t, kinematic ay vy y t, kinematic az vz z t)

distance :: Vec -> Integer
distance (x, y, z) = x+y+z

compareVecDist :: Particle -> Particle -> Ordering
compareVecDist (_, a1, v1, x1) (_, a2, v2, x2) = compare (distance new1) (distance new2)
        where (new1, new2) = ((timeFunc a1 v1 x1 50000000), (timeFunc a2 v2 x2 50000000))

{- 
 - Yes, I know there are much better ways to do this, but I need to catch up on the other puzzles
 - and this is a very easy solution.
 - In theory, one, given the v and a vectors, could simply determine the limit of their d ratios as
 - t approaches infinity analytically. However, due to time constraints and the fact that I have
 - other things to do, I will not be doing this. Generally, this leads to the particle with
 - the lowest acceleration winning, but its much easier to plug in "50 million" and be done.
-}

numberP :: Particle -> Int
numberP (num, _, _, _) = num

filterParticle :: String -> String
filterParticle str = filter (\x->(isDigit x) || (x == '-')) str

parseParticle :: String -> Int -> Particle
parseParticle str n = let (x:y:z:vx:vy:vz:ax:ay:az:[]) = (map (read) ((words.filterParticle) str))
        in (n, (x, y, z), (vx, vy, vz), (ax, ay, az))

parse :: [String] -> Int -> [Particle]
parse [] _ = []
parse (x:xs) n = (parseParticle x n):(parse xs (n+1))

solve1 :: String -> Int
solve1 str = numberP (head (sortBy compareVecDist (parse (lines str) 0)))

main = ((readFile "day_20_input.txt") >>= (putStrLn.show.solve1))

