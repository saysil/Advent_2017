
import Data.IntSet

type Tape = IntSet

type Position = Int
data StateFunction = STF (Tape -> Position -> (Tape, Position, StateFunction))

type Turing = ((Tape, Position, StateFunction), Int)

writeOne :: Tape -> Position -> Tape
writeOne = (flip insert)

writeZero :: Tape -> Position -> Tape
writeZero = (flip delete)

searchTape :: Tape -> Position -> Bool
searchTape = (flip member)

runA :: Tape -> Position -> (Tape, Position, StateFunction)
runA t pos = case searchTape t pos of
        False -> (writeOne t pos, pos+1, STF runB)
        True -> (writeZero t pos, pos-1, STF runC)

runB :: Tape -> Position -> (Tape, Position, StateFunction)
runB t pos = case searchTape t pos of
        False -> (writeOne t pos, pos-1, STF runA)
        True -> (t, pos+1, STF runC)

runC :: Tape -> Position -> (Tape, Position, StateFunction)
runC t pos = case searchTape t pos of
        False -> (writeOne t pos, pos+1, STF runA)
        True -> (writeZero t pos, pos-1, STF runD)

runD :: Tape -> Position -> (Tape, Position, StateFunction)
runD t pos = case searchTape t pos of
        False -> (writeOne t pos, pos-1, STF runE)
        True -> (t, pos-1, STF runC)

runE :: Tape -> Position -> (Tape, Position, StateFunction)
runE t pos = case searchTape t pos of
        False -> (writeOne t pos, pos+1, STF runF)
        True -> (t, pos+1, STF runA)

runF :: Tape -> Position -> (Tape, Position, StateFunction)
runF t pos = case searchTape t pos of
        False -> (writeOne t pos, pos+1, STF runA)
        True -> (t, pos+1, STF runE)

stepTuring :: (Tape, Position, StateFunction) -> (Tape, Position, StateFunction)
stepTuring (t, pos, STF f) = f t pos

runUntilChecksum :: Int -> Turing -> Turing
runUntilChecksum stop (t, n) = case n == stop of
        False -> runUntilChecksum stop (stepTuring t, n+1)
        True -> (t, n)

getTape :: Turing -> Tape
getTape ((t, _, _), _) = t

solve1 :: Int -> Int
solve1 stop = (size.getTape) (runUntilChecksum stop ((empty, 0, STF runA), 0))

main = (putStrLn.show.solve1) 12134527

