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

alter :: Eq k => (Maybe v -> Maybe v) -> k -> [(k, v)] -> [(k, v)]
alter func key [] = case func Nothing of
    Nothing -> []
    Just value -> [(key, value)]
alter func key ((key', value'):xs)
    | key == key' = case func (Just value') of
        Nothing -> xs
        Just newValue -> (key', newValue) : xs
    | otherwise = (key', value') : (alter func key xs)
