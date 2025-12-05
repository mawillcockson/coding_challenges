module Graph (
    hasNode,
    addNode,
    ) where

type DiGraph a = [(a, [a])]

member :: Eq a => a -> DiGraph a -> Bool
member _ [] = False
member key ((key', _):xs) = key == key' || (member key xs)

hasNode :: Eq a => DiGraph a -> a -> Bool
hasNode = flip member

addNode :: Eq a => DiGraph a -> a -> DiGraph a
addNode graph element
    | graph `hasNode` element = graph
    | otherwise = (element, []) : graph
