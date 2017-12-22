
-- Recreate tree given only leaf nodes and nodes above them
-- O(nk) - For each level on the tree, iterate through the list of remaining leaves.
-- Start with the top level and remove elements as they are found to have children in higher
-- level trees.
-- Ex: [a, b, c->f, d->e], [e, f] -> [a, b], [c, d]


import Data.HashMap as HM
import Data.Char

data PTree = PT (Int, String) [PTree]

getName :: PTree -> String
getName (PT (weight, name) _) = name


topToPTree :: String -> (Int, [String]) -> PTree
topToPTree name (weight, _) = PT (weight, name) []


getTopLevel :: Map String (Int, [String]) -> [PTree]
getTopLevel xs = elems $ mapWithKey topToPTree (HM.filter (Prelude.null.snd) xs)

matchElement :: [PTree] -> String -> (Int, [String]) -> Maybe PTree
matchElement xs name (w, children)
        | Prelude.null matches = Nothing
        | otherwise = Just (PT (w, name) matches) 
        where matches = Prelude.filter (((flip elem) children).getName) xs

constructTree :: Map String (Int, [String]) -> [PTree] -> [PTree]
constructTree tree xs 
        | HM.null matchedTree = xs
        | otherwise = constructTree (difference tree matchedTree) (elems $ matchedTree)
        where matchedTree = mapMaybeWithKey (matchElement xs) tree

filterLine :: Char -> Bool
filterLine c = (isAlphaNum c) || (c == ' ')

parseLine :: [String] -> Map String (Int, [String]) -> Map String (Int, [String])
parseLine [] tree = tree
parseLine (x:xs) tree = let (name:weight:links) = (words (Prelude.filter filterLine x))
        in parseLine xs (insert name (read weight, links) tree)

solve1 :: String -> String
solve1 str = getName $ (head (constructTree init (getTopLevel init)))
        where init = parseLine (lines str) empty

--main = (putStrLn.show) (words (Prelude.filter filterLine "fwft (72) -> ktlj, cntj, xhth"))
main = ((readFile "day_7_input.txt") >>= (putStrLn.show.solve1))
