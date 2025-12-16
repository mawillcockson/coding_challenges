module Ladder (
    test,
    Dictionary,
    readDictionary,
    -- readDictionaryV2,
) where

import qualified Data.Char (isLowerCase)
import Data.Function ((&))
import qualified Data.List
import qualified Data.PermutationMap as PM
import Debug.Trace (traceShowId)
import qualified Graph as G

type Dictionary = [String]

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
