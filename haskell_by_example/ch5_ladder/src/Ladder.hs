module Ladder (
    test,
    Dictionary,
    readDictionary,
    -- readDictionaryV2,
) where

import qualified Data.Char (isLowerCase)
import qualified Data.Enum
import Data.Function ((&))
import qualified Data.HashSet as HashSet
import qualified Data.Hashable
import qualified Data.List
import qualified Data.PermutationMap as PM
import Debug.Trace (traceShowId)
import qualified Graph as G

type Dictionary = [String]

-- We want two words that should have a mapping, to have the same key in the
-- permutation map. This way, when e.g. cat is looked up, depending upon the
-- allowed transformations for a single "move", we could find a mapping of cat
-- to:
-- - Reorder: act
-- - Remove1: at
-- - Add1   : cats
-- - Swap1  : hat
data SingleMoveTransformation
    = Swap Int -- cat -> hat => at = drop 1 of every
    | Add Int -- cat -> cats => cat -> cats = self and drop 1 of every
    | Remove Int -- cat -> at => cat -> at = self and drop 1 of every
    | Reorder -- cat -> act => act = sort
    deriving (Show, Eq, Data.Enum.Enum)

instance Data.Hashable.Hashable SingleMoveTransformation where
    hashWithSalt = Data.Hashable.hashUsing Data.Enum.fromEnum

test :: IO ()
test = do
    putStrLn $ "V1 -> " ++ (makeDictionary "reacted\ncreated\neat\nate\ntea\nPlato\nJohn\njohn" & unlines)
    putStrLn $ mkLadderGraph (words "cat cats act dog") & show

-- putStrLn $ "V2 -> " ++ (makeDictionaryV2 "reacted\ncreated\neat\nate\ntea\nPlato\nJohn\njohn" & unlines)

readDictionary :: FilePath -> IO Dictionary
readDictionary filePath = do
    dictionaryContent <- readFile filePath
    return $ makeDictionary dictionaryContent

makeDictionary :: String -> Dictionary
makeDictionary content =
    let lnes = Data.List.lines content
        wrds = Data.List.map (Data.List.filter (`Data.List.elem` ['a' .. 'z'])) lnes
     in Data.List.nub wrds

readDictionaryV2 :: FilePath -> IO Dictionary
readDictionaryV2 filePath = do
    dictionaryContent <- readFile filePath
    return $ makeDictionaryV2 dictionaryContent

makeDictionaryV2 :: String -> Dictionary
makeDictionaryV2 content =
    let lnes = Data.List.lines content
        wrds = Data.List.filter (all Data.Char.isLowerCase) lnes
     in Data.List.nub wrds

mkLadderGraph :: Dictionary -> G.DiGraph String
mkLadderGraph dictionary = G.buildDiGraph nodes
  where
    permutationMap = PM.createPermutationMap dictionary
    nodes = Data.List.map (\w -> (w, computeCandidates permutationMap w)) dictionary

{-
mkLadderGraphV2 :: Dictionary -> G.DiGraph String
mkLadderGraphV2 dictionary = G.buildDiGraph nodes
  where
    nodes = Data.List.map (\word -> (word, computeCandidatesV2 dictionary word)) dictionary
-}

lowerCaseLetters :: [Char]
lowerCaseLetters = ['a' .. 'z']

computeCandidates :: PM.PermutationMap -> String -> [String]
computeCandidates graph word =
    let candidates = modified ++ added ++ removed ++ [word]
        uniques = Data.List.nub [Data.List.sort word | word <- candidates]
        permutations = Data.List.concatMap (\x -> PM.findWithDefault graph [] x) uniques
     in Data.List.delete word permutations
  where
    added = [x : word | x <- lowerCaseLetters]
    removed = [Data.List.delete x word | x <- word]
    modified = [x : Data.List.delete y word | x <- lowerCaseLetters, y <- word, x /= y]

{-
computeCandidatesV2 :: Dictionary -> String -> [String]
computeCandidatesV2 dictionary word = Data.List.delete word candidates
  where
    candidates = Data.List.filter (isValidWord) permutations
    permutations = Data.List.concatMap (allSwaps word) lowerCaseLetters
    allSwaps word letter =
-}

alternate :: [a] -> [a] -> [a]
alternate left right = go Left' left right
  where
    go :: Side -> [a] -> [a] -> [a]
    go _side [] right' = right'
    go Left' (left' : lefts) rights = left' : go Right' lefts rights
    go Right' lefts (right' : rights) = right' : go Left' lefts rights
    go _side left' [] = left'

addEveryLetter :: [Char] -> String -> [String]
addEveryLetter letters string = (string :) $ go letters string & Data.List.concat
  where
    len = Data.List.length string
    explode :: [String]
    explode = map (\c -> [c]) string
    empty :: [String]
    empty = Data.List.replicate (len + 1) ""
    go :: String -> String -> [[String]]
    go [] _string = []
    go (letter : letters') string' =
        let insertions = [appendAt letter pos empty | pos <- [1 .. len + 1]]
            assemble :: [String] -> String
            assemble insertion = alternate insertion explode & Data.List.concat
         in (map (assemble) insertions) : go letters' string'

permuteAdd :: [Char] -> Int -> String -> [String]
permuteAdd letters count string
    | count < 0 = undefined
    | count == 0 = [string]
    | otherwise = permuteAdd letters (count - 1) string & map (addEveryLetter letters) & Data.List.concat

lowerCaseLetters :: [Char]
lowerCaseLetters = "abcdefghijklmnopqrstuvwxyz"

permuteAddLowercase :: Int -> String -> [String]
permuteAddLowercase = permuteAdd lowerCaseLetters

data Carrier a
    = Replaced a
    | Original a
    deriving (Show, Eq)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

type Replaced a = [Carrier a]
type ReplacedString = [Carrier Char]

carry :: [a] -> Replaced a
carry = map (Original)

extractChar :: Carrier a -> a
extractChar (Replaced a) = a
extractChar (Original a) = a

extractString :: [Carrier a] -> [a]
extractString = map (extractChar)

extractAllStrings :: [[Carrier a]] -> [[a]]
extractAllStrings = map (extractString)

swapAllWith :: [Char] -> ReplacedString -> [ReplacedString]
swapAllWith letters replacedString =
    [ (take pos replacedString) ++ [Replaced letter] ++ (drop (pos + 1) replacedString)
    | (pos, Original c) <- enumerate replacedString
    , letter <- letters
    , c /= letter
    ]

permuteSwap :: [Char] -> Int -> String -> [String]
permuteSwap letters swapCount string = extractAllStrings $ go swapCount
  where
    original :: ReplacedString
    original = carry string

    go :: Int -> [ReplacedString]
    go 1 = original : swapAllWith letters original
    go count
        | count < 0 = [original]
        | otherwise = original : concat (map (swapAllWith letters) $ go $ count - 1)

permute :: SingleMoveTransformation -> String -> [String]
permute Reorder word = [Data.List.sort word]
permute (Add n) word = permuteAddLowercase n word
permute (Remove n) word =
    let len = Data.List.length word
        drop1Right (left, right) = Data.List.concat [left, Data.List.drop 1 right]
     in [drop1Right $ Data.List.splitAt x word | x <- [0 .. (len - 1)]]
permute (Swap n) word = permuteSwap lowerCaseLetters n word
