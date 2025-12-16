module Data.FactoredPermutationMap (
    test,
    Permutation (..),
    Node (..),
    FactoredPermutationMap,
    empty,
    member,
    alter,
    delete,
    upsert,
    Data.FactoredPermutationMap.lookup,
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
import qualified Data.HashSet as HashSet
import qualified Data.Hashable
import qualified Data.List
import qualified Data.Maybe
import qualified Data.Text

test :: IO ()
test = do
    let upsertF :: String -> [String] -> FactoredPermutationMap -> FactoredPermutationMap
        upsertF key values graph = upsert graph (Permutation key) (map (Node) values)
        example :: FactoredPermutationMap
        example = empty & upsertF "cab" ["cat", "dab", "cob"] & upsertF "cat" ["act"] & upsertF "at" ["cat", "hat"]
    putStrLn $ "example -> " ++ (show example)
    putStrLn $ "addWords (\w -> "

newtype Permutation = Permutation String deriving (Eq, Show)
newtype Node = Node String deriving (Eq, Show)
type FactoredPermutationMap = AM.AssocMap Permutation [Node]

empty :: FactoredPermutationMap
empty = AM.empty

strLower :: String -> String
strLower = Data.Text.unpack . Data.Text.toCaseFold . Data.Text.pack

strSort :: String -> String
strSort = Data.List.sort . strLower

lowerCaseLetters :: [Char]
lowerCaseLetters = ['a' .. 'z']

member :: Permutation -> FactoredPermutationMap -> Bool
member = AM.member

alter :: FactoredPermutationMap -> (Maybe [Node] -> Maybe [Node]) -> Permutation -> FactoredPermutationMap
alter = AM.alter

delete :: FactoredPermutationMap -> Permutation -> FactoredPermutationMap
delete = AM.delete

upsert :: FactoredPermutationMap -> Permutation -> [Node] -> FactoredPermutationMap
upsert = AM.upsert

lookup :: FactoredPermutationMap -> Permutation -> Maybe [Node]
lookup = AM.lookup

findWithDefault :: FactoredPermutationMap -> [Node] -> Permutation -> [Node]
findWithDefault = AM.findWithDefault

type CompareEqual = Node -> Node -> Bool

addWords :: CompareEqual -> FactoredPermutationMap -> [Permutation] -> [Node] -> FactoredPermutationMap
addWords compareEqual graph keys wrds = Data.List.foldl' (alter') graph keys
  where
    alter' :: FactoredPermutationMap -> Permutation -> FactoredPermutationMap
    alter' graph' key = AM.alter graph' (insertWord) key
    insertWord :: Maybe [Node] -> Maybe [Node]
    insertWord Nothing = Just wrds
    insertWord (Just wrds') = Just $ Data.List.unionBy compareEqual wrds' wrds

type Permute = Node -> [Permutation]

addWord :: Permute -> CompareEqual -> FactoredPermutationMap -> Node -> FactoredPermutationMap
addWord permute compareEqual graph word = addWords compareEqual graph (permute word) [word]

createPermutationMap :: Permute -> CompareEqual -> [Node] -> FactoredPermutationMap
createPermutationMap permute compareEqual = Data.List.foldl' (addWord permute compareEqual) empty
