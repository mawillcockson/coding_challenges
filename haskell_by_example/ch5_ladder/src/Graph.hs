module Graph (
    main,
    hasNode,
    newNode,
    addEdge,
    addEdges,
    addNode,
    buildDiGraph,
    ) where

import qualified Data.AssocMap as AM
import qualified Data.Maybe (fromJust)
import Data.Function ((&))
import qualified Data.List (union, nub)

type DiGraph a = AM.AssocMap a [a]

main :: IO ()
main = do
    putStrLn $ "addEdges [(1,1),(2,2)] -> " ++ (addEdges [(1,1),(2,2)] (empty) & show)
    putStrLn $ "addEdges [(1,1),(2,2),(2,3),(3,3),(3,2)] -> " ++ (addEdges [(1,1),(2,2),(2,3),(3,3),(3,2)] (empty) & show)
    putStrLn $ "buildDiGraph [(1,[1]),(2,[2,3]),(3,[3,2])] -> " ++ (buildDiGraph [(1,[1]),(2,[2,3]),(3,[3,2])] & show)
    putStrLn $ "buildDiGraph [(1,[1]),(2,[2,3]),(3,[3,2]),(1,[2]),(2,[2,3,3,4,4])] -> " ++ (buildDiGraph [(1,[1]),(2,[2,3]),(3,[3,2]),(1,[2]),(2,[2,3,3,4,4])] & show)

empty :: DiGraph a
empty = AM.empty

member :: Eq a => a -> DiGraph a -> Bool
member key graph = AM.member key graph

hasNode :: Eq a => DiGraph a -> a -> Bool
hasNode = flip member

newNode :: Eq a => DiGraph a -> a -> DiGraph a
newNode graph node
    | graph `hasNode` node = graph
    | otherwise = AM.insert graph node [] & Data.Maybe.fromJust

addEdge :: Eq a => DiGraph a -> (a, a) -> DiGraph a
{-
addEdge graph (startNode, endNode) =
    let intermediateGraph = newNode graph startNode
        connectedNodes = AM.lookup intermediateGraph startNode & Data.Maybe.fromJust
        modifiedConnections = Data.List.union connectedNodes [endNode]
        newGraph = AM.update intermediateGraph startNode modifiedConnections & Data.Maybe.fromJust
    in newGraph
-}
addEdge graph (startNode, endNode) = addNode graph (startNode, [endNode])

addEdges :: Eq a => [(a, a)] -> DiGraph a -> DiGraph a
addEdges edges graph = foldl' (addEdge) graph edges

addNode :: Eq a => DiGraph a -> (a, [a]) -> DiGraph a
addNode graph (startNode, endNodes) =
    let intermediateGraph = newNode graph startNode
        connectedNodes = AM.lookup intermediateGraph startNode & Data.Maybe.fromJust
        modifiedConnections = Data.List.union connectedNodes endNodes
        newGraph = AM.update intermediateGraph startNode modifiedConnections & Data.Maybe.fromJust
    in newGraph

buildDiGraph :: Eq a => [(a, [a])] -> DiGraph a
buildDiGraph = foldl' (addNode) (empty)

children :: Eq a => DiGraph a -> a -> [a]
children graph key = AM.findWithDefault graph [] key
