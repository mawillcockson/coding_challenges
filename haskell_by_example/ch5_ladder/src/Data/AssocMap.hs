module Data.AssocMap (
    AssocMap,
    member,
    alter,
    empty,
    delete,
    upsert,
    update,
    insert,
    Data.AssocMap.lookup,
    ) where

import Data.Maybe (fromJust)
import Data.Function ((&))

main :: IO ()
main = do
    let insertF key value graph = fromJust $ insert graph key value
        example :: AssocMap Int Int
        example = empty & insertF 1 1 & insertF 2 2
        ins = insert example
        upd = update example
        ups = upsert example
        del = delete example
        expectJust = show . fromJust
        expectNothing :: (Show k, Show v) => Maybe (AssocMap k v) -> String
        expectNothing = maybe "Nothing" (show)
    putStrLn $ "Example map: " ++ (show example)
    putStrLn $ "Insert {3: 3} -> " ++ (ins 3 3 & expectJust)
    putStrLn $ "Insert {2: 3} -> " ++ (ins 2 3 & expectNothing)
    putStrLn $ "Update {2: 3} -> " ++ (upd 2 3 & expectJust)
    putStrLn $ "Update {3: 3} -> " ++ (upd 3 3 & expectNothing)
    putStrLn $ "Upsert {2: 3} -> " ++ (ups 2 3 & show)
    putStrLn $ "Upsert {3: 3} -> " ++ (ups 3 3 & show)
    putStrLn $ "Delete {2: 3} -> " ++ (del 2 & show)
    putStrLn $ "Delete {3: 3} -> " ++ (del 3 & show)

newtype AssocMap k v = AssocMap [(k, v)]
    deriving (Show)

member :: Eq k => k -> AssocMap k v -> Bool
member key (AssocMap xs) = member' key xs
    where
        member' :: Eq a => a -> [(a, b)] -> Bool
        member' element lst = maybe False (const True) (Prelude.lookup element lst)

alter :: Eq k => AssocMap k v -> (Maybe v -> Maybe v) -> k -> AssocMap k v
alter (AssocMap graph) function key = AssocMap $ alter' function key graph
    where
        alter' :: Eq k => (Maybe v -> Maybe v) -> k -> [(k, v)] -> [(k, v)]
        alter' func key' [] = maybe ([]) (\value -> [(key', value)]) (func Nothing)
        alter' func key' ((key'', value''):xs)
            | key' == key'' = maybe (xs) (\newValue -> (key'', newValue) : xs) (func (Just value''))
            | otherwise = (key'', value'') : (alter' func key' xs)

empty :: AssocMap k v
empty = AssocMap []

delete :: Eq k => AssocMap k v -> k -> AssocMap k v
delete graph key = alter graph (const Nothing) key

upsert :: Eq k => AssocMap k v -> k -> v -> AssocMap k v
upsert graph key value = alter graph (const $ Just value) key

update :: Eq k => AssocMap k v -> k -> v -> Maybe (AssocMap k v)
update graph key value = if key `member` graph
    then Just $ alter graph (maybe undefined (const $ Just value)) key
    else Nothing

insert :: Eq k => AssocMap k v -> k -> v -> Maybe (AssocMap k v)
insert graph key value = if key `member` graph
    then Nothing
    else Just $ alter graph (maybe (Just value) undefined) key

lookup :: Eq k => AssocMap k v -> k -> Maybe v
lookup (AssocMap graph) key = lookup' key graph
    where
        lookup' _ [] = Nothing
        lookup' key' ((key'', value'):xs)
            | key' == key'' = Just value'
            | otherwise = lookup' key' xs
