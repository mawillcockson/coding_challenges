module Graph (
    main,
    hasNode,
    newNode,
    addEdge,
    addEdges,
    addNode,
    children,
    buildDiGraph,
    deleteEdge,
    deleteNode,
    ) where

import qualified Data.AssocMap as AM
import qualified Data.Maybe (fromJust)
import Data.Function ((&))
import qualified Data.List (foldl', union, nub, (\\))

type DiGraph a = AM.AssocMap a [a]

main :: IO ()
main = do
    putStrLn $ "addEdges [(1,1),(2,2)] -> " ++ (addEdges [(1,1),(2,2)] (empty :: DiGraph Int) & show)
    putStrLn $ "addEdges [(1,1),(2,2),(2,3),(3,3),(3,2)] -> " ++ (addEdges [(1,1),(2,2),(2,3),(3,3),(3,2)] (empty :: DiGraph Int) & show)
    putStrLn $ "buildDiGraph [(1,[1]),(2,[2,3]),(3,[3,2])] -> " ++ ((buildDiGraph [(1,[1]),(2,[2,3]),(3,[3,2])] :: DiGraph Int) & show)
    putStrLn $ "buildDiGraph [(1,[1]),(2,[2,3]),(3,[3,2]),(1,[2]),(2,[2,3,3,4,4]),(4,[4]),(5,[5])] -> " ++ ((buildDiGraph [(1,[1]),(2,[2,3]),(3,[3,2]),(1,[2]),(2,[2,3,3,4,4]),(4,[4]),(5,[5])] :: DiGraph Int) & show)
    let example = buildDiGraph [(1,[1]),(2,[2,3]),(3,[3,2]),(1,[2]),(2,[2,3,3,4,4]),(4,[4]),(5,[5])] :: DiGraph Int
    putStrLn "^ -> example"
    putStrLn $ "deleteEdge example 1 2 -> " ++ (deleteEdge example 1 2 & show)
    putStrLn $ "deleteEdge example 4 4 -> " ++ (deleteEdge example 4 4 & show)
    putStrLn $ "deleteNode example 2 -> " ++ (deleteNode example 2 & show)

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
addEdges edges graph = Data.List.foldl' (addEdge) graph edges

addNode :: Eq a => DiGraph a -> (a, [a]) -> DiGraph a
addNode graph (startNode, endNodes) = AM.alter graph (insertNode) startNode where
    insertNode Nothing = Just endNodes
    insertNode (Just nodes) = Just $ Data.List.union nodes endNodes

buildDiGraph :: Eq a => [(a, [a])] -> DiGraph a
buildDiGraph = Data.List.foldl' (addNode) (empty)

children :: Eq a => DiGraph a -> a -> [a]
children graph key = AM.findWithDefault graph [] key

deleteEdge :: Eq a => DiGraph a -> a -> a -> DiGraph a
deleteEdge graph startNode endNode = AM.alter graph (aux) startNode where
    aux Nothing = Nothing
    aux (Just [node]) = Nothing
    aux (Just nodes) = Just $ nodes Data.List.\\ [endNode]

-- BUG: actually need to remove node from every other node in the graph
-- Because the graph is directed, just because one node is connected to
-- another, doesn't mean that it will appear in the other node's connections
deleteNode :: Eq a => DiGraph a -> a -> DiGraph a
deleteNode graph startNode = AM.delete withoutNodeGraph startNode where
    connectedNodes = AM.lookup graph startNode & Data.Maybe.fromJust
    withoutNodeGraph = Data.List.foldl' (\graph' node -> deleteEdge graph' node startNode) graph connectedNodes
