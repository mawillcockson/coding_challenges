module Graph (
    hasNode,
    addNode,
    ) where

import qualified Data.AssocMap (member)

type DiGraph a = [(a, [a])]

hasNode :: Eq a => DiGraph a -> a -> Bool
hasNode = flip Data.AssocMap.member

addNode :: Eq a => DiGraph a -> a -> DiGraph a
addNode graph element
    | graph `hasNode` element = graph
    | otherwise = (element, []) : graph
