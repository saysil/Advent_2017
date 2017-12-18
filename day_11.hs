
-- Reduce Hexagonal Directions

{-# LANGUAGE ViewPatterns #-}

import Data.List

data NSDir = N | S deriving (Eq, Show)

data EWDir = E | W deriving (Eq, Show)

class Dir a where
        opposite :: a -> a

op :: (Dir a) => a -> a
op = (opposite)

instance Dir NSDir where
        opposite N = S
        opposite S = N

instance Dir EWDir where
        opposite W = E
        opposite E = W

data HexDir = Vertical NSDir | Lateral NSDir EWDir deriving (Eq, Show)

instance Dir HexDir where
        opposite (Vertical a) = Vertical (opposite a)
        opposite (Lateral a b) = Lateral (opposite a) (opposite b)

readH :: String -> HexDir
readH "nw" = Lateral N W
readH "ne" = Lateral N E
readH "n"  = Vertical N
readH "s"  = Vertical S
readH "sw" = Lateral S W
readH "se" = Lateral S E

commasToSpaces :: String -> String
commasToSpaces [] = []
commasToSpaces (x:xs)
        | x == ',' = ' ':(commasToSpaces xs)
        | otherwise = x:(commasToSpaces xs)

parseHex :: String -> [HexDir]
parseHex a = reverse $ map readH (words (commasToSpaces a))


lat :: HexDir -> EWDir
lat (Lateral _ a) = a
lat _ = error "Called lateral on vertical value"

-- Returns true for (lateral movements only) opposite lateral movements and same vertical movements
latCancel :: NSDir -> EWDir -> HexDir -> Bool
latCancel vd1 ld1 (Lateral vd2 ld2) = ((op ld1) == ld2) && (vd1 == vd2)
latCancel _ _ _ = False

-- Returns true for (lateral movements only) opposite vertical movements
vertCancel :: NSDir -> HexDir -> Bool
vertCancel vdir1 (Lateral vdir2 ldir) = (op vdir1) == vdir2
vertCancel _ _ = False

-- Returns true for equal lateral movements and opposite vertical movements
loopCancel :: HexDir -> HexDir -> Bool
loopCancel (Lateral vd1 ld1) (Lateral vd2 ld2) = ((op vd1) == vd2) && (ld1 == ld2)
loopCancel _ _ = False

-- We make proven reductions on the path until it is reduced to its simplest form
-- These are:
-- Opposite Reduction   :: (Any) <- (~Any) = Nothing
-- Vertical Preference  :: V Dir <- V = V <- V Dir
-- Lateral Cancelling   :: V Dir <- V (Op Dir) = V
-- Vertical Cancelling  :: V Dir <- (~V) = (~V) Dir
-- Loop Destruction (v) :: V <- V Dir <- (~V) Dir = V Dir <- V Dir
-- Loop Destruction (l) :: V Dir <- V Dir <- (~V) Dir = V Dir <- (~V) Dir <- V Dir

reduce :: [HexDir] -> [HexDir]
-- Dir <- op Dir -> [] :: Opposite reduction
reduce (a:((== op a) -> True):xs) = reduce xs
-- S Dir <- S -> (S <- S Dir) :: Vertical Preference
reduce (a@(Lateral vd ld):b@((== Vertical vd) -> True):xs) = reduce (b:a:xs)
-- South Dir <- South (Op Dir) -> South :: Lateral Cancelling
reduce ((Lateral vd ld):((latCancel vd ld) -> True):xs) = reduce ((Vertical vd):xs)
-- North Dir <- South -> South Dir :: Vertical Cancelling (1)
reduce ((Lateral vd ld):((== Vertical (op vd)) -> True):xs) = reduce ((Lateral (op vd) ld):xs)
-- South <- North Dir -> South Dir :: Vertical Cancelling (2)
reduce ((Vertical vd):b@((vertCancel vd) -> True):xs) = reduce ((Lateral vd (lat b)):xs)
-- N <- N Dir <- S Dir -> (N Dir <- N Dir) :: Loop Destruction (Vertical)
reduce ((Vertical vd):(b@((vertCancel (op vd)) -> True)):((loopCancel b) -> True):xs) = reduce (b:b:xs)
-- S Dir <- S Dir <- N Dir -> (S Dir <- N Dir <- S Dir) :: Loop Destruction (Lateral)
reduce (a@(Lateral vd ld):((== a) -> True):((loopCancel a) -> True):xs) = reduce (a:(Lateral (op vd) ld):a:xs)
reduce (x:xs) = x:(reduce xs)
reduce [] = []

reduceList :: [HexDir] -> [HexDir]
reduceList a 
        | (reduced == a) = a
        | otherwise = reduceList reduced
        where reduced = reduce a

solve1 :: String -> Int
solve1 = (length.reduceList.parseHex)

makeList :: [HexDir] -> [HexDir] -> [Int]
makeList copy (x:xs) = (length newList):(makeList newList xs)
        where newList = (reverse (reduceList (x:(reverse (copy)))))
makeList _ [] = []

solve2 :: String -> Int
solve2 str = maximum (map (length.reduceList) (tails (parseHex str)))
--solve2 = (maximum.(makeList []).parseHex)

main = ((readFile "day_11_input.txt") >>= (putStrLn.show.solve2))
--main = (putStrLn.show) (solve2 "ne,ne,sw,sw")

