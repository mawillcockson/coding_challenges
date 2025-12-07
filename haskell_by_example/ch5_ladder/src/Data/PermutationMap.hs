module Data.PermutationMap (
    main,
    PermutationMap,
    empty,
    member,
    alter,
    delete,
    upsert,
    Data.PermutationMap.lookup,
    findWithDefault,
    createPermutationMap,
    ) where

import qualified Data.AssocMap as AM (
    AssocMap,
    empty,
    member,
    alter,
    upsert,
    delete,
    lookup,
    findWithDefault,)
import Data.Function ((&))
import qualified Data.List (sort, union, foldl')
import qualified Data.Text (toCaseFold, pack, unpack)

main :: IO ()
main = do
    let upsertF :: String -> [String] -> PermutationMap -> PermutationMap
        upsertF key value graph = upsert graph key value
        example :: PermutationMap
        example = empty & upsertF "abc" ["acc","abb"] & upsertF "def" ["ddf"]
        ups = upsert example
        del = delete example
    putStrLn $ "Example map: " ++ (show example)
    putStrLn $ "Upsert abc [acc,adc] -> " ++ (ups "abc" ["acc","adc"] & show)
    putStrLn $ "Upsert cab [adc,dbc] -> " ++ (ups "cab" ["adc","dbc"] & show)
    putStrLn $ "Delete abc -> " ++ (del "abc" & show)
    putStrLn $ "Delete hij -> " ++ (del "hij" & show)

type PermutationMap = AM.AssocMap String [String]

empty :: PermutationMap
empty = AM.empty

strLower :: String -> String
strLower = Data.Text.unpack . Data.Text.toCaseFold . Data.Text.pack

strSort :: String -> String
strSort = Data.List.sort . strLower

member :: String -> PermutationMap -> Bool
member = AM.member . strSort

alter :: PermutationMap -> (Maybe [String] -> Maybe [String]) -> String -> PermutationMap
alter graph function key = AM.alter graph function (strSort key)

delete :: PermutationMap -> String -> PermutationMap
delete = flip $ flip AM.delete . strSort

upsert :: PermutationMap -> String -> [String] -> PermutationMap
upsert = flip $ flip AM.upsert . strSort

lookup :: PermutationMap -> String -> Maybe [String]
lookup = flip $ flip AM.lookup . strSort

findWithDefault :: PermutationMap -> [String] -> String -> [String]
findWithDefault permutations defaultValue key = AM.findWithDefault permutations defaultValue $ strSort key

addWord :: PermutationMap -> String -> PermutationMap
addWord graph word = AM.alter graph (insertWord) $ strSort word where
    lowerCased = strLower word
    insertWord Nothing = Just [lowerCased]
    insertWord (Just wrds) = Just $ Data.List.union wrds [lowerCased]

createPermutationMap :: [String] -> PermutationMap
createPermutationMap = Data.List.foldl' (addWord) empty
