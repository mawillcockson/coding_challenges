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
    alter,
    delete,
    empty,
    findWithDefault,
    lookup,
    member,
    upsert,
 )
import Data.Function ((&))
import qualified Data.List (foldl', sort, union)
import qualified Data.Text (pack, toCaseFold, unpack)

main :: IO ()
main = do
    let upsertF :: String -> [String] -> PermutationMap -> PermutationMap
        upsertF key value graph = upsert graph key value
        example :: PermutationMap
        example = empty & upsertF "abc" ["acc", "abb"] & upsertF "def" ["ddf"]
        ups = upsert example
        del = delete example
        add = addWord example
        adds = addWords example
    putStrLn $ "Example map: " ++ (show example)
    putStrLn $ "Upsert cab [adc,dbc] -> " ++ (ups "cab" ["adc", "dbc"] & show)
    putStrLn $ "Delete abc -> " ++ (del "abc" & show)
    putStrLn $ "Delete hij -> " ++ (del "hij" & show)
    putStrLn $ "addWords abc [acc,adc] -> " ++ (adds "abc" ["acc", "adc"] & show)
    putStrLn $ "addWords cab [adc,dbc] -> " ++ (adds "cab" ["adc", "dbc"] & show)
    putStrLn $ "addWord cab -> " ++ (add "cab" & show)
    putStrLn $ "addWord dbc -> " ++ (add "dbc" & show)
    putStrLn $ "createPermutationMap [abc,cab,bac,cad,dac,adc] -> " ++ (createPermutationMap ["abc", "cab", "bac", "cad", "dac", "adc"] & show)

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

addWords :: PermutationMap -> String -> [String] -> PermutationMap
addWords graph word wrds = AM.alter graph (insertWord) $ strSort word
  where
    lowerCased = map strLower wrds
    insertWord Nothing = Just lowerCased
    insertWord (Just wrds') = Just $ Data.List.union wrds' lowerCased

addWord :: PermutationMap -> String -> PermutationMap
addWord graph word = addWords graph word [word]

createPermutationMap :: [String] -> PermutationMap
createPermutationMap = Data.List.foldl' (addWord) empty
