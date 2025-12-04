module Graph where

type DiGraph a = [(a, [a])]

member :: Eq a => a -> [(a, b)] -> Bool
member _ [] = False
member element ((x, _) : xs)
    | x == element = True
    | otherwise = member element xs

hasNode :: Eq a => DiGraph a -> a -> Bool
hasNode = flip member

addNode :: Eq a => DiGraph a -> a -> DiGraph a
addNode graph element
    | graph `hasNode` element = graph
    | otherwise = (element, []) : graph
