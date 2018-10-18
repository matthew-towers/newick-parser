import Text.ParserCombinators.Parsec
import Data.Either

------------------------------------------------
--Weighted Tree data type and helper functions--
------------------------------------------------

data WeightedTree a b = Node a [(WeightedTree a b, b)] deriving Show
-- the list is the trees whose root nodes are direct descendants of the
-- node.  The second element of the pair is the weight of the edge
-- between the node and the root node of the descendant.

labs :: WeightedTree a b -> [a]
labs (Node a ls) = a : (concat (map (\ x -> labs (fst x)) ls))
--not good, see Bird 7.5 p.162

test :: WeightedTree String Int
test = Node "a" [ (Node "e" [], 58), (Node "r" [], 98), (Node "b" [(Node "d" [], 4), (Node "c" [], 3)], 74)]

isNode :: (Eq a) => a -> WeightedTree a b -> Bool
-- isNode x tr asks if x is the label of a node in tr
isNode x w = x `elem` (labs w)

weightToNode :: (Eq a, Num b) => a -> WeightedTree a b -> b
-- find the weight/distance of the path from the root of the second
-- argument to the node labelled by the first argument
weightToNode x (Node y ls)
    | x == y = 0
    | otherwise = sum (map (acc x) ls)

acc :: (Num b, Eq a) => a -> (WeightedTree a b, b) -> b
acc x (Node e ls, r)
    | not (isNode x (Node e ls)) = 0
    | x == e = r
    | otherwise = r + sum (map (acc x) ls)

mrca :: Eq a => a -> a -> WeightedTree a b -> WeightedTree a b
-- mrca x y tr returns the subtree of tr which is the smallest
-- containing both x and y
mrca x y (Node e ls)
    | null subtreeswithxy = Node e ls
    | otherwise = mrca x y (fst (head subtreeswithxy))
 where subtreeswithxy = filter (\ t -> ((isNode x (fst t)) && (isNode y (fst t)))) ls

dist :: (Eq a, Num b) => a -> a -> WeightedTree a b -> b
--dist x y tr finds the distance from x to y in the weighted tree tr
dist x y tr = (weightToNode x sub) + (weightToNode y sub)
    where sub = mrca x y tr

---------------
--Parser code--
---------------

basicpair :: GenParser Char st (WeightedTree String Int, Int)
-- parses abc:54 into (Node "abc" [], 54)
basicpair = do
    firstpart <- many (noneOf ",:()")
    char ':'
    secondpart <- many digit 
    return (Node firstpart [], read secondpart :: Int)

parseCommaList = sepBy (try basicpair <|> newickInner) (char ',')
-- parses a comma-separated list of Newick reps of weighted trees into a
-- list 

newickInner :: GenParser Char st (WeightedTree String Int, Int)
--main Newick format parser, dealing with strings of the form
--(comma sep list of Newick reps of weighted trees):weight
newickInner = do
    char '('
    bps <- parseCommaList
    string "):"
    natu <- many digit
    return (Node "" bps, read natu:: Int)

newick :: GenParser Char st (WeightedTree String Int)
--deals with the outermost part of Newick rep, which is
--(comma seperated list of Newick reps of weighted trees);
newick = do
    char '('
    bps <- parseCommaList
    string ");"
    return (Node "" bps)

parInner :: String -> Either ParseError (WeightedTree String Int, Int)
parInner input = parse newickInner "(unknown)" input

par :: String -> Either ParseError (WeightedTree String Int)
--make it easier to call the parser
par input = parse newick "(unknown)" input
