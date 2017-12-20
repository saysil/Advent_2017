
import Control.Monad
import Data.Sequence as S
import Data.List

data MapSquare = VPipe | HPipe | Cross | Letter Char deriving Eq

data Direction = North | South | East | West

type Position = (Int, Int)

type Board = (Seq (Seq (Maybe MapSquare)), Position, Direction)

walk :: Direction -> Board -> Board
walk North (sq, (x, y), dir) = (sq, (x, y-1), North)
walk South (sq, (x, y), dir) = (sq, (x, y+1), South)
walk East  (sq, (x, y), dir) = (sq, (x+1, y), East)
walk West  (sq, (x, y), dir) = (sq, (x-1, y), West)

crossWalk :: Direction -> Board -> [Board]
crossWalk North b = map ($b) (map walk [East, West])
crossWalk South b = map ($b) (map walk [East, West])
crossWalk East b  = map ($b) (map walk [North, South])
crossWalk West b  = map ($b) (map walk [North, South])

walkPath :: Board -> [Char]
walkPath board@(squares, (x, y), dir) = case join ((S.lookup y squares) >>= (S.lookup x)) of
        Nothing -> []
        Just VPipe -> walkPath (walk dir board)
        Just HPipe -> walkPath (walk dir board)
        Just (Letter a) -> a:(walkPath (walk dir board))
        Just Cross -> foldl (++) [] (map walkPath (crossWalk dir board))
        -- Call on all adjacent squares

parseChar :: Char -> Maybe MapSquare
parseChar '|' = Just VPipe
parseChar '-' = Just HPipe
parseChar ' ' = Nothing
parseChar '+' = Just Cross
parseChar x   = Just (Letter x)

parse :: String -> [[Maybe MapSquare]]
parse str = (map.map) (parseChar) (lines str)

getEntry :: [[Maybe MapSquare]] -> Int
getEntry squares = case (elemIndex (Just VPipe) (squares!!0)) of
        Nothing -> error "Could not find entry"
        Just a -> a

toBoard :: [[Maybe MapSquare]] -> Board
toBoard squares = ((fromList (map fromList squares)), ((getEntry squares), 0), South)

solve1 :: String -> [Char]
solve1 str = walkPath $ (toBoard.parse) str

main = ((readFile "day_19_input.txt") >>= (putStrLn.show.solve1))

