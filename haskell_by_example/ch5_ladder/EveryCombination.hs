module EveryCombination (main) where

import Data.Function ((&))
import qualified Data.List
import Data.Maybe (mapMaybe)

letterAtPos :: Int -> Int -> Char -> [String]
letterAtPos remaining position letter
    | remaining <= 0 = []
    | remaining == position = [letter] : letterAtPos (remaining - 1) position letter
    | otherwise = [] : letterAtPos (remaining - 1) position letter

letterAtPosV2 :: Char -> Int -> [String] -> [String]
letterAtPosV2 letter position lst
    | position <= 0 = lst
    | otherwise =
        let len = Data.List.length lst
            fromLeft = len - position
         in if position > len then lst else go letter fromLeft lst
  where
    go :: Char -> Int -> [String] -> [String]
    go _letter _pos' [] = []
    go letter' pos' (string : strings)
        | pos' == 0 = (string ++ [letter']) : go letter' (pos' - 1) strings
        | otherwise = string : go letter' (pos' - 1) strings

appendAt :: a -> Int -> [[a]] -> [[a]]
appendAt letter position lst
    | position <= 0 = lst
    | otherwise =
        let len = Data.List.length lst
            fromLeft = len - position
         in if position > len then lst else go letter fromLeft lst
  where
    go :: a -> Int -> [[a]] -> [[a]]
    go _letter _pos' [] = []
    go letter' pos' (string : strings)
        | pos' == 0 = (string ++ [letter']) : go letter' (pos' - 1) strings
        | otherwise = string : go letter' (pos' - 1) strings

data Side = Left' | Right'

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

replaceAt :: a -> Int -> [a] -> [a]
replaceAt letter position lst
    | position < 0 = lst
    | otherwise =
        let len = Data.List.length lst
            fromLeft = (len - 1) - position
         in if position > len then lst else go letter fromLeft lst
  where
    go :: a -> Int -> [a] -> [a]
    go _newX _pos' [] = []
    go newX pos' (x : xs)
        | pos' == 0 = newX : go newX (pos' - 1) xs
        | otherwise = x : go newX (pos' - 1) xs

swapEveryLetter :: [Char] -> String -> [String]
swapEveryLetter letters string = (string :) $ go letters & Data.List.concat
  where
    len = Data.List.length string
    go :: String -> [[String]]
    go [] = []
    go (letter : letters') = [replaceAt letter pos string | pos <- [0 .. len - 1]] : go letters'

data Carrier a
    = Replaced a
    | Original a

extract :: Carrier a -> a
extract (Replaced a) = a
extract (Original a) = a

extractAll :: [Carrier a] -> [a]
extractAll = map (extract)

permuteSwap :: [Char] -> Int -> String -> [String]
permuteSwap letters count string = map (extractAll) $ go letters count
  where
    chars :: [Carrier Char]
    chars = map (Original) string
    len = Data.List.length chars

    maybeReplaceAt :: (Eq a) => a -> Int -> [Carrier a] -> Maybe [Carrier a]
    maybeReplaceAt replacement position lst
        | position < 0 = undefined
        | otherwise =
            let len = Data.List.length lst
             in if position >= len
                    then undefined
                    else aux replacement position lst []
      where
        aux :: (Eq a) => a -> Int -> [Carrier a] -> [Carrier a] -> Maybe [Carrier a]
        aux _replacement _pos [] newLst = Just newLst
        aux replacement' 0 (x : xs) newLst = case x of
            Replaced _a -> Nothing
            Original a ->
                if a == replacement'
                    then Nothing
                    else aux replacement' (0 - 1) xs ((Replaced replacement') : newLst)
        aux replacement' pos (x : xs) newLst = aux replacement' pos xs (x : newLst)

    makeReplacements :: Char -> [Carrier Char] -> [[Carrier Char]]
    makeReplacements letter' string' = mapMaybe (\pos -> maybeReplaceAt letter' pos string') [0 .. len - 1]

    go :: [Char] -> Int -> [[Carrier Char]]
    go (letter : letters') count'
        | count' < 0 = undefined
        | count' == 0 = [chars]
        | otherwise = go letters (count - 1) & map (makeReplacements letter) & Data.List.concat

main :: IO ()
main = do
    print $ letterAtPos 3 1 'a'
    print $ letterAtPos 3 2 'a'
    print $ letterAtPos 3 3 'a'
    print $ ["a", "", ""] & 'b' `letterAtPosV2` 3 & 'c' `letterAtPosV2` 3
    print $ ["a", "", ""] & 'a' `letterAtPosV2` 4 & 'b' `letterAtPosV2` 3 & 'c' `letterAtPosV2` 3 & 'd' `letterAtPosV2` 2 & 'e' `letterAtPosV2` 2 & 'f' `letterAtPosV2` 1 & 'g' `letterAtPosV2` 0
    print $ ["a", "", ""] & 'a' `appendAt` 4 & 'b' `appendAt` 3 & 'c' `appendAt` 3 & 'd' `appendAt` 2 & 'e' `appendAt` 2 & 'f' `appendAt` 1 & 'g' `appendAt` 0
    print $ addEveryLetter lowerCaseLetters "abc"
    print $ Data.List.length $ addEveryLetter lowerCaseLetters "abc" & map (addEveryLetter lowerCaseLetters) & Data.List.concat
    -- print $ Data.List.length $ addEveryLetter lowerCaseLetters "abc" & map (addEveryLetter lowerCaseLetters) & Data.List.concat & map (addEveryLetter lowerCaseLetters) & Data.List.concat
    print $ Data.List.length $ permuteAddLowercase 2 "abc"
    -- print $ permuteAddLowercase 2 "abc"
    print $ "abc" & 'z' `replaceAt` 0 & 'x' `replaceAt` 2
    let swaps = swapEveryLetter lowerCaseLetters "abc"
    print swaps
    putStrLn $ "-> " ++ (show $ Data.List.length swaps)
    putStrLn $ "& nub -> " ++ (show $ Data.List.nub swaps & Data.List.length)
    let newSwaps = permuteSwap lowerCaseLetters 1 "abc"
    print newSwaps
    putStrLn $ "-> " ++ (show $ Data.List.length newSwaps)
    putStrLn $ "& nub -> " ++ (show $ Data.List.nub newSwaps & Data.List.length)
    putStrLn $ "swapEveryLetter 2 letters abc & nub -> " ++ (swapEveryLetter lowerCaseLetters "abc" & map (swapEveryLetter lowerCaseLetters) & Data.List.concat & Data.List.nub & Data.List.length & show)

-- putStrLn $ "permuteSwap lowerCaseLetters 2 abc -> " ++ (permuteSwap lowerCaseLetters 2 "abc" & Data.List.length & show)
