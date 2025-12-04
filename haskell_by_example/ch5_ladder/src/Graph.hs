module Graph (
    member,
    hasNode,
    addNode,
    alter,
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
alter func key [] = maybe ([]) (\value -> [(key, value)]) (func Nothing)
alter func key ((key', value'):xs)
    | key == key' = maybe (xs) (\newValue -> (key', newValue) : xs) (func (Just value'))
    | otherwise = (key', value') : (alter func key xs)
