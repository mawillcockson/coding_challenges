module Graph (
    member,
    hasNode,
    addNode,
    ) where

type DiGraph a = [(a, [a])]

member :: Eq a => a -> [(a, b)] -> Bool
member element lst = maybe False (const True) (lookup element lst)

hasNode :: Eq a => DiGraph a -> a -> Bool
hasNode = flip member

addNode :: Eq a => DiGraph a -> a -> DiGraph a
addNode graph element
    | graph `hasNode` element = graph
    | otherwise = (element, []) : graph
