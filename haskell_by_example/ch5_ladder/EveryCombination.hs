module EveryCombination (main) where

import Data.Function ((&))
import Data.List ((!?))
import qualified Data.List
import Data.Maybe (catMaybes)

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
    deriving (Show, Eq)

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

{- much simpler version available
enumerate :: [a] -> [(Int, a)]
enumerate lst = go 0 lst
  where
    go :: Int -> [a] -> [(Int, a)]
    go _i [] = []
    go i (x : xs) = (i, x) : (go (i + 1) xs)
-}

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

maybeReplaceAt :: (Eq a) => a -> Int -> Replaced a -> Maybe (Replaced a)
maybeReplaceAt replacement position lst
    | position < 0 = undefined
    | otherwise =
        let len' = Data.List.length lst
            fromLeft = (len' - 1) - position
         in if position >= len'
                then undefined
                else aux replacement fromLeft lst []
  where
    aux :: (Eq a) => a -> Int -> Replaced a -> Replaced a -> Maybe (Replaced a)
    aux _replacement _pos [] newLst = Just $ Data.List.reverse newLst
    aux replacement' 0 (x : xs) newLst = case x of
        Replaced _a -> Nothing
        Original a ->
            if a == replacement'
                then Nothing
                else aux replacement' (0 - 1) xs ((Replaced replacement') : newLst)
    aux replacement' pos (x : xs) newLst = aux replacement' (pos - 1) xs (x : newLst)

allSwappingsOfLetter :: (Eq a) => a -> Replaced a -> [Replaced a]
allSwappingsOfLetter letter string = Data.Maybe.catMaybes $ [maybeReplaceAt letter pos string | pos <- [0 .. (len - 1)]]
  where
    len = Data.List.length string

allSwappings :: [Char] -> ReplacedString -> [ReplacedString]
allSwappings letters string = concatMap (flip allSwappingsOfLetter $ string) letters

{-
permuteSwap :: [Char] -> Int -> String -> [String]
permuteSwap letters count string = string : (extractAllStrings $ go count)
  where
    chars :: ReplacedString
    chars = map (Original) string

    go :: Int -> [ReplacedString]
    go count'
        | count' < 0 = undefined
        | count' == 0 = [chars]
        | otherwise = go (count' - 1) & map (allSwappings letters) & Data.List.concat
-}

swapAllWith :: [Char] -> ReplacedString -> [ReplacedString]
swapAllWith letters replacedString =
    [ (take pos replacedString) ++ [Replaced letter] ++ (drop (pos + 1) replacedString)
    | (pos, Original c) <- enumerate replacedString
    , letter <- letters
    , c /= letter
    ]

-- NOTE::BUG This still does extra work, and produces duplicates
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

permuteRemove :: Int -> String -> [String]
permuteRemove removeCount string =
    let result = go removeCount
     in (fst result) ++ (snd result)
  where
    go :: Int -> ([String], [String])
    go count
        | count <= 0 = ([], [string])
        | otherwise =
            let (overPreviousStep, previousStep) = go (count - 1)
             in ( overPreviousStep ++ previousStep
                , [ (take pos word) ++ (drop (pos + 1) word)
                  | word <- previousStep
                  , pos <- [0 .. (Data.List.length word) - 1]
                  ]
                )

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
    let example = "abc"
        swaps = swapEveryLetter lowerCaseLetters example
        otherSwaps = swapAllWith lowerCaseLetters (carry example) & extractAllStrings
    print swaps
    print otherSwaps
    putStrLn $ "swaps -> " ++ (show $ Data.List.length swaps)
    -- putStrLn $ "swaps & nub -> " ++ (show $ Data.List.nub swaps & Data.List.length)
    putStrLn $ "otherSwaps -> " ++ (show $ Data.List.length otherSwaps)
    -- putStrLn $ "otherSwaps & nub -> " ++ (show $ Data.List.nub otherSwaps & Data.List.length)
    let newSwaps = permuteSwap lowerCaseLetters 1 example
    -- print newSwaps
    putStrLn $ "newSwaps -> " ++ (show $ Data.List.length newSwaps)
    -- putStrLn $ "newSwaps & nub -> " ++ (show $ Data.List.nub newSwaps & Data.List.length)
    putStrLn $ "maybeReplaceAt 'z' 2 $ map (Original) abc -> " ++ (show $ maybeReplaceAt 'z' 2 $ map (Original) "abc")
    putStrLn $ "maybeReplaceAt 'a' 2 [Original c | c <- abc] -> " ++ (show $ maybeReplaceAt 'a' 2 [Original c | c <- "abc"])
    putStrLn $ "catMaybes $ [maybeReplaceAt 'z' pos abc | pos <- [0 .. len - 1]] -> " ++ (show $ catMaybes $ [maybeReplaceAt 'z' pos (map (Original) "abc") | pos <- [0 .. 3 - 1]])
    -- putStrLn $ "swapEveryLetter 2 letters abc & nub -> " ++ (swapEveryLetter lowerCaseLetters "abc" & map (swapEveryLetter lowerCaseLetters) & Data.List.concat & Data.List.nub & Data.List.length & show)
    putStrLn $ "permuteSwap lowerCaseLetters 2 abc -> " ++ (permuteSwap lowerCaseLetters 2 "abc" & Data.List.length & show)
    let twoSwap = swapEveryLetter lowerCaseLetters "ab" & map (swapEveryLetter lowerCaseLetters) & Data.List.concat
    putStrLn $ "swapEveryLetter 2 letters ab -> " ++ (twoSwap & Data.List.length & show)
    -- putStrLn $ "swapEveryLetter 2 letters ab & nub -> " ++ (twoSwap & Data.List.nub & Data.List.length & show)
    let twoSwap = permuteSwap lowerCaseLetters 2 "ab"
    putStrLn $ "permuteSwap lowerCaseLetters 2 ab -> " ++ (twoSwap & Data.List.length & show)
    -- putStrLn $ "permuteSwap lowerCaseLetters 2 ab & nub -> " ++ (twoSwap & Data.List.nub & Data.List.length & show)
    putStrLn $ "26 ^ 2 -> " ++ (show $ 26 ** 2 & round)
    print $ permuteSwap "+=" 1 "ab"
    print $ permuteSwap "+=" 2 "ab"
    print $ swapAllWith lowerCaseLetters (carry "abc") & map (swapAllWith lowerCaseLetters) & (++ [[carry "abc"]]) & concat & extractAllStrings & Data.List.length
    print $ swapAllWith "+=" (carry "ab")
    print $ swapAllWith "+=" (carry "ab") & map (swapAllWith "#@") & concat
    print $ swapAllWith "+=" (carry "ab") & map (swapAllWith "+=") & concat
    print $ permuteRemove 1 "abc"
    print $ permuteRemove 2 "abc"
