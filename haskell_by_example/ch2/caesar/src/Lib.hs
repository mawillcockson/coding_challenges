module Lib
    ( square,
    Alphabet,
    lowerAlphabet,
    upperAlphabet,
    digits,
    isLower,
    isUpper,
    isDigit,
    isMisc,
    listLength,
    indexOf,
    (!-!),
    rotateChar,
    rotate,
    rotateWithAlphabet,
    lowerRot,
    upperRot,
    digitRot,
    rotateCharGuard,
    caesar,
    rot13,
    rot135,
    count,
    letterFrequency,
    guessShift,
    showGuessedShift,
    worksForEveryShift,
    wrapAroundMod,
    ) where

square :: Int -> Int
square x = x * x

type Alphabet = [Char]

lowerAlphabet :: Alphabet
lowerAlphabet = ['a' .. 'z']

upperAlphabet :: Alphabet
upperAlphabet = ['A' .. 'Z']

digits :: Alphabet
digits = ['0' .. '9']

isLower :: Char -> Bool
isLower character = character `elem` lowerAlphabet

isUpper :: Char -> Bool
isUpper character = character `elem` upperAlphabet

isDigit :: Char -> Bool
isDigit character = character `elem` digits

isMisc :: Char -> Bool
{-
isMisc character = not (
    isLower character
    || isUpper character
    || isDigit character
)
-}
isMisc character = character `notElem` (
    lowerAlphabet
    ++ upperAlphabet
    ++ digits)

listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

indexOf :: Eq a => a -> [a] -> Int
indexOf character (x : xs) = if x == character
    then 0
    else 1 + indexOf character xs
indexOf _ [] = undefined

(!-!) :: [a] -> Int -> a
(!-!) (x:_) 0 = x
(!-!) (_:xs) index = if index < 0 then undefined else xs !-! (index - 1)
(!-!) [] _ = undefined

rotateChar :: Int -> Char -> Char
rotateChar offset character = if isUpper character
    then upperAlphabet !-! (((character `indexOf` upperAlphabet) + offset) `mod` (listLength upperAlphabet))
    else if isLower character
        then lowerAlphabet !-! (((character `indexOf` lowerAlphabet) + offset) `mod` (listLength lowerAlphabet))
        else if isDigit character
            then digits !-! (((character `indexOf` digits) + offset) `mod` (listLength digits))
            else character

rotate :: Int -> String -> String
rotate _ [] = []
rotate offset (x:xs) = (rotateChar offset x) : (rotate offset xs)

rotateWithAlphabet :: Alphabet -> Int -> Char -> Char
rotateWithAlphabet alphabet offset character = alphabet !-! (((character `indexOf` alphabet) + offset) `mod` (listLength alphabet))

lowerRot :: Int -> Char -> Char
lowerRot = rotateWithAlphabet lowerAlphabet
upperRot :: Int -> Char -> Char
upperRot = rotateWithAlphabet upperAlphabet
digitRot :: Int -> Char -> Char
digitRot = rotateWithAlphabet digits

rotateCharGuard :: Int -> Char -> Char
rotateCharGuard offset character
    | isUpper character = upperRot offset character
    | isLower character = lowerRot offset character
    | isDigit character = digitRot offset character
    | otherwise = character

caesar :: Int -> String -> String
caesar _ [] = []
caesar offset message = map (\character -> rotateCharGuard offset character) message

rot13 :: String -> String
rot13 message = caesar 13 message

rot135 :: String -> String
rot135 message = map (symmetricallyRotateByType) message where
    symmetricallyRotateByType character
        | isUpper character = upperRot (halfLength upperAlphabet) character
        | isLower character = lowerRot (halfLength lowerAlphabet) character
        | isDigit character = digitRot (halfLength digits) character
        | otherwise = character
    halfLength :: [a] -> Int
    halfLength list = (listLength list) `div` 2

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count element (x:xs) = (if element == x then 1 else 0) + (count element xs)

letterFrequency :: Alphabet -> String -> [Double]
letterFrequency alphabet string = map (\char -> fromIntegral (count char string) / total) alphabet where
    total = fromIntegral (listLength string)

guessShift :: String -> Int
guessShift message = maybeEIndex - eIndex where
    eIndex = 'e' `indexOf` lowerAlphabet
    maybeEIndex = mostCommonLetter `indexOf` lowerAlphabet
    mostCommonLetter = lowerAlphabet !-! mostCommonLetterIndex
    mostCommonLetterIndex = largestFrequency `indexOf` frequencies
    largestFrequency = maximum frequencies
    frequencies = letterFrequency lowerAlphabet message

showGuessedShift :: String -> String
showGuessedShift encoded = caesar (-(guessShift encoded)) encoded

wrapAroundMod :: Integral a => a -> a -> a
wrapAroundMod num modulus = if remainder < 0 then remainder + modulus else remainder where
    remainder = num `mod` modulus

worksForEveryShift :: (String -> Int) -> String -> Bool
worksForEveryShift guesser message = all (\x -> x == True) shiftsThatGuessCorrectly where
    shiftsThatGuessCorrectly = [(guesser (caesar shift message)) `wrapAroundMod` 26 == shift | shift <- [0..shiftMax]]
    shiftMax = (listLength lowerAlphabet) - 1
