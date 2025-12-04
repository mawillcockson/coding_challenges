module Data.AssocMap (
    AssocMap,
    member,
    alter,
    ) where

newtype AssocMap k v = AssocMap [(k, v)]

member :: Eq k => k -> AssocMap k v -> Bool
member key (AssocMap xs) = member' key xs
    where
        member' :: Eq a => a -> [(a, b)] -> Bool
        member' element lst = maybe False (const True) (lookup element lst)

alter :: Eq k => (Maybe v -> Maybe v) -> k -> AssocMap k v -> AssocMap k v
alter function key (AssocMap graph) = AssocMap $ alter' function key graph
    where
        alter' :: Eq k => (Maybe v -> Maybe v) -> k -> [(k, v)] -> [(k, v)]
        alter' func key' [] = maybe ([]) (\value -> [(key', value)]) (func Nothing)
        alter' func key' ((key'', value''):xs)
            | key' == key'' = maybe (xs) (\newValue -> (key'', newValue) : xs) (func (Just value''))
            | otherwise = (key'', value'') : (alter' func key' xs)
