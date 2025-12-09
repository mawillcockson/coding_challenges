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
import qualified Data.Enum
import Data.Function ((&))
import qualified Data.HashSet as HashSet
import qualified Data.Hashable
import qualified Data.List
import qualified Data.Maybe
import qualified Data.Text

main :: IO ()
main = do
    let upsertF :: String -> [String] -> PermutationMap -> PermutationMap
        upsertF key value graph = upsert graph key value
        example :: PermutationMap
        example = empty & upsertF "cab" ["cat", "dab", "cob"] & upsertF "cat" ["act"] & upsertF "at" ["cat", "hat"]
        memberCheck = member $ HashSet.fromList [Swap1, Reorder]
    putStrLn $ "hat" `memberCheck` example & show

type PermutationMap = AM.AssocMap String [String]

-- We want two words that should have a mapping, to have the same key in the
-- permutation map. This way, when e.g. cat is looked up, depending upon the
-- allowed transformations for a single "move", we could find a mapping of cat
-- to:
-- - Reorder: act
-- - Remove1: at
-- - Add1   : cats
-- - Swap1  : hat
data SingleMoveTransformation
    = Swap1 -- cat -> hat => at = drop 1 of every
    | Add1 -- cat -> cats => cat -> cats = self and drop 1 of every
    | Remove1 -- cat -> at => cat -> at = self and drop 1 of every
    | Reorder -- cat -> act => act = sort
    deriving (Show, Eq, Data.Enum.Enum)

instance Data.Hashable.Hashable SingleMoveTransformation where
    hashWithSalt = Data.Hashable.hashUsing Data.Enum.fromEnum

empty :: PermutationMap
empty = AM.empty

strLower :: String -> String
strLower = Data.Text.unpack . Data.Text.toCaseFold . Data.Text.pack

strSort :: String -> String
strSort = Data.List.sort . strLower

lowerCaseLetters :: [Char]
lowerCaseLetters = ['a' .. 'z']

permute :: SingleMoveTransformation -> String -> [String]
permute Reorder word = [Data.List.sort word]
permute Add1 word =
    let len = Data.List.length word
        splits = [Data.List.splitAt x word | x <- [0 .. len]]
        insertAtEveryPos letter = map (insertLetter letter) splits
        insertLetter letter (left, right) = Data.List.concat [left, [letter], right]
     in Data.List.concatMap (insertAtEveryPos) lowerCaseLetters
permute Remove1 word =
    let len = Data.List.length word
        drop1Right (left, right) = Data.List.concat [left, Data.List.drop 1 right]
     in [drop1Right $ Data.List.splitAt x word | x <- [0 .. (len - 1)]]
permute Swap1 word =
    let len = Data.List.length word
        splits = [Data.List.splitAt x word | x <- [0 .. (len - 1)]]
        swap1Right letter (left, (_ : rest)) = Just $ Data.List.concat [left, letter : rest]
        swap1Right _letter (_left, []) = Nothing
        swapAtEveryPos letter = Data.Maybe.mapMaybe (swap1Right letter) splits
     in Data.List.concatMap (swapAtEveryPos) lowerCaseLetters

member :: HashSet.HashSet SingleMoveTransformation -> String -> PermutationMap -> Bool
member transforms word graph = any (\lookup -> word `lookup` graph) lookups
  where
    lookups = map (toFunc) $ HashSet.toList transforms
    toFunc transform word' graph' = or [transformed `AM.member` graph' | transformed <- permute transform word']

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
