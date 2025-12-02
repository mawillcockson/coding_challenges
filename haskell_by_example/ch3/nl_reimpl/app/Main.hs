module Main (
    main,
    mainV1,
    interactiveLinesV1,
    interactiveLinesV2,
    doReturn,
    repeatUpper,
    readLines,
    numberAllLinesV1,
    isEmpty,
    isNotEmpty,
    numberLines,
    numberAllLines,
    numberNonEmptyLines,
    numberAndIncrementNonEmptyLines,
    incrementNonEmptyAndNumberAllLines,
    PadMode(..),
    pad,
    padLeft,
    padRight,
    padCenter,
    zip,
    unzip,
    zipWith,
    zipV2,
    prettyNumberedLines,
    mapM,
    mapM_,
    mainV2,
    mainV3,
    parseArgumentsV2,
    count,
    ) where

import Prelude hiding (zip, unzip, zipWith, mapM, mapM_)
import qualified Data.Char (toUpper, isPrint, isSeparator)
import qualified System.Environment (getProgName, getArgs)
import qualified Data.Either (lefts, rights, either)

main :: IO ()
main = mainV3

interactiveLinesV1 :: IO ()
interactiveLinesV1 = do
    line <- getLine
    putStrLn ("1. " ++ line)

doReturn :: IO Int
doReturn = do
    _ <- return (1 :: Int)
    return (2 :: Int)

interactiveLinesV2 :: Int -> IO ()
interactiveLinesV2 index = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn (show index ++ ". " ++ line)
            interactiveLinesV2 (index + 1)

repeatUpper :: IO ()
repeatUpper = do
    line <- getLine
    putStrLn (map Data.Char.toUpper line)

printHelpText :: String -> IO ()
printHelpText msg = do
    putStrLn (msg ++ "\n")
    progName <- System.Environment.getProgName
    putStrLn ("Usage: " ++ progName ++ "<options> <filename>")
    putStrLn "\n"
    putStrLn " Options:"
    putStrLn "   --reverse          - Reverse the numbering"
    putStrLn "   --skip-empty       - Skip numbering empty lines"
    putStrLn "   --no-count-empty   - Don't count empty lines"
    putStrLn "   --left-align       - Use left-aligned line numbers"

parseArgumentsV1 :: [String] -> Maybe FilePath
parseArgumentsV1 [filePath] = Just filePath
parseArgumentsV1 _ = Nothing

mainV1 :: IO ()
mainV1 = do
    cliArgs <- System.Environment.getArgs
    let mFilePath = parseArgumentsV1 cliArgs
    maybe
        (printHelpText "Missing filename")
        (\filePath -> putStrLn filePath)
        mFilePath

readLines :: FilePath -> IO [String]
readLines filePath = do
    contents <- readFile filePath
    return (lines contents)

type NumberedLine = (Maybe Integer, String)
type NumberedLines = [NumberedLine]

numberAllLinesV1 :: [String] -> NumberedLines
numberAllLinesV1 strings =
    let helper :: Integer -> [String] -> NumberedLines
        helper _ [] = []
        helper index (line:lnes) = (Just index, line) : (helper (index + 1) lnes)
    in helper 0 strings

isEmpty :: String -> Bool
isEmpty str = null str ||
    all (\c -> not (Data.Char.isPrint c) || Data.Char.isSeparator c) str

isNotEmpty :: String -> Bool
isNotEmpty str = not $ isEmpty str

numberLines :: (String -> Bool) -> (String -> Bool) -> [String] -> NumberedLines
numberLines shouldIncr shouldNumber strings =
    let aux :: Integer -> [String] -> NumberedLines
        aux _ [] = []
        aux index (str:strs) =
            let mNumber = if shouldNumber str
                then Just index
                else Nothing
                newIndex = if shouldIncr str
                    then index + 1
                    else index
                in (mNumber, str) : (aux newIndex strs)
        in aux 1 strings

numberAllLines :: [String] -> NumberedLines
numberAllLines = numberLines (const True) (const True)

numberNonEmptyLines :: [String] -> NumberedLines
numberNonEmptyLines = numberLines (const True) isNotEmpty

numberAndIncrementNonEmptyLines :: [String] -> NumberedLines
numberAndIncrementNonEmptyLines = numberLines isNotEmpty isNotEmpty

incrementNonEmptyAndNumberAllLines :: [String] -> NumberedLines
incrementNonEmptyAndNumberAllLines = numberLines isNotEmpty (const True)

data PadMode =
    PadLeft
    | PadRight
    | PadCenter

pad :: PadMode -> Int -> String -> String
pad mode amount str =
    let diff = amount - length str
        padding = replicate diff ' '
    in case mode of
        PadLeft -> padding ++ str
        PadRight -> str ++ padding
        PadCenter -> let half = replicate (diff `div` 2) ' '
                         other = replicate (diff - (diff `div` 2)) ' '
                        in half ++ str ++ other

padLeft :: Int -> String -> String
padLeft = pad PadLeft

padRight :: Int -> String -> String
padRight = pad PadRight

padCenter :: Int -> String -> String
padCenter = pad PadCenter

zip :: [a] -> [b] -> [(a, b)]
zip _ [] = []
zip [] _ = []
zip (x:xs) (y:ys) = (x, y) : (zip xs ys)

unzip :: [(a, b)] -> ([a], [b])
unzip associatedList =
    let aux :: [a] -> [b] -> [(a, b)] -> ([a], [b])
        aux collectX collectY ((x, y):xys) = aux (collectX ++ [x]) (collectY ++ [y]) xys
        aux collectX collectY _ = (collectX, collectY)
    in aux [] [] associatedList

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ _ [] = []
zipWith _ [] _ = []
zipWith combiner (x:xs) (y:ys) = (combiner x y) : (zipWith combiner xs ys)

zipV2 :: [a] -> [b] -> [(a, b)]
zipV2 = zipWith (\x y -> (x, y))

prettyNumberedLines :: PadMode -> NumberedLines -> [String]
prettyNumberedLines mode numberLnes =
    let (numbers, lnes) = unzip numberLnes
        numberStrs = map (maybe "" show) numbers
        padAmount = maximum $ map (length) numberStrs
        paddedNumbers = map (pad mode padAmount) numberStrs
    in zipWith (\num lne -> num ++ " " ++ lne) paddedNumbers lnes

mapM :: (a -> IO b) -> [a] -> IO [b]
mapM action subjects =
    let aux :: [b] -> (a -> IO b) -> [a] -> IO [b]
        aux ys _ [] = return ys
        aux ys act (x:xs) = do
            y <- act x
            aux (ys ++ [y]) act xs
    in aux [] action subjects

mapM_ :: (a -> IO b) -> [a] -> IO ()
mapM_ _ [] = return ()
mapM_ action (x:xs) = do
    _ <- action x
    mapM_ action xs

mainV2 :: IO ()
mainV2 = do
    cliArgs <- System.Environment.getArgs
    let mFilePath = parseArgumentsV1 cliArgs
    maybe
        (printHelpText "Missing filename")
        (\filePath -> do
            fileLines <- readLines filePath
            let numberedLines = numberAllLines fileLines
                prettyNumbered = prettyNumberedLines PadLeft numberedLines
            mapM_ (putStrLn) prettyNumbered
        )
        mFilePath

data ProgramOption
    = ReverseNumbering
    | SkipEmptyLines
    | LeftAlign
    | NoIncrementEmpty
    deriving (Eq, Show)

lnOptionFromString :: String -> Either String ProgramOption
lnOptionFromString "--reverse" = Right ReverseNumbering
lnOptionFromString "--skip-empty" = Right SkipEmptyLines
lnOptionFromString "--no-count-empty" = Right NoIncrementEmpty
lnOptionFromString "--left-align" = Right LeftAlign
lnOptionFromString option = Left option

count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count classifier (x:xs)
    | classifier x = 1 + (count classifier xs)
    | otherwise = count classifier xs

parseArgumentsV2 :: [String] -> Either String (FilePath, [ProgramOption])
parseArgumentsV2 arguments =
    let parsed = map lnOptionFromString arguments
        allLefts = Data.Either.lefts parsed
    in case allLefts of
        [filePath] -> Right (filePath, Data.Either.rights parsed)
        unkownOptions -> Left $ "expected exactly 1 filename; instead got " ++ (show unkownOptions)

-- I want to ride the ROP train:
-- https://fsharpforfunandprofit.com/rop/
-- https://tgdwyer.github.io/eithers/#example
mainV3 :: IO ()
mainV3 = do
    cliArgs <- System.Environment.getArgs
    let eFilePathAndOptions = parseArgumentsV2 cliArgs
    Data.Either.either
        (printHelpText)
        (\(filePath, options) -> do
            fileLines <- readLines filePath
            let numberFunc = case (SkipEmptyLines `elem` options, NoIncrementEmpty `elem` options) of
                    (True, True) -> numberAndIncrementNonEmptyLines
                    (True, False) -> numberNonEmptyLines
                    (False, True) -> incrementNonEmptyAndNumberAllLines
                    (False, False) -> numberAllLines
                reverseOrNo = if ReverseNumbering `elem` options
                    then reverse
                    else id
                numberedLines = numberFunc $ reverseOrNo fileLines
                mode = if LeftAlign `elem` options
                    then PadRight
                    else PadLeft
                prettyNumbered = reverseOrNo $ prettyNumberedLines mode numberedLines
            mapM_ (putStrLn) prettyNumbered
            putStrLn $ show options
        )
        eFilePathAndOptions
