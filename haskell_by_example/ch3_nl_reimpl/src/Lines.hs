module Lines (example, lines, unlines, words, unwords) where

import Prelude hiding (lines, unlines, words, unwords)

example :: IO ()
example = do
    linesTest
    unlinesTest
    wordsTest
    unwordsTest

linesTest :: IO ()
linesTest = do
    putStr "lines \"a\\nb\\nc\" -> "
    putStrLn $ show $ lines "a\nb\nc"
    putStrLn $ "test: " ++ (show $ lines "a\nb\nc")
    putStr "lines \"\\n\\na\\nb\\n\\nc\\n\\n\" -> "
    putStrLn $ show $ lines "\n\na\nb\n\nc\n\n"
    putStrLn $ "test: " ++ (show $ lines "\n\na\nb\n\nc\n\n")

unlinesTest :: IO ()
unlinesTest = do
    putStr "unlines [\"a\",\"b\",\"c\"] -> "
    putStrLn $ unlines ["a","b","c"]
    putStrLn $ "test: " ++ (unlines ["a","b","c"])

wordsTest :: IO ()
wordsTest = do
    putStr "words \"a b c\" -> "
    putStrLn $ show $ words "a b c"
    putStrLn $ "test: " ++ (show $ words "a b c")
    putStr "words \"  a b  c  \" -> "
    putStrLn $ show $ words "  a b  c  "
    putStrLn $ "test: " ++ (show $ words "  a b  c  ")

unwordsTest :: IO ()
unwordsTest = do
    putStr "unwords [\"a\",\"b\",\"c\"] -> "
    putStrLn $ unwords ["a","b","c"]
    putStrLn $ "test: " ++ (unwords ["a","b","c"])

lines :: String -> [String]
lines str =
    let aux :: [Char] -> String -> [String]
        aux word ['\n'] = [word]
        aux word [] = [word]
        aux word (char:chars)
            | char == '\n' = word : (aux [] chars)
            | otherwise = aux (word ++ [char]) chars
        in aux [] str

unlines :: [String] -> String
unlines [] = ""
unlines (lne:lnes) = lne ++ "\n" ++ (unlines lnes)

words :: String -> [String]
words str =
    let aux :: [Char] -> String -> [String]
        aux word (char:chars)
            | char == ' ' && word == "" = aux word chars
            | char == ' ' = word : (aux [] chars)
            | otherwise = aux (word ++ [char]) chars
        aux "" [] = []
        aux word [] = [word]
        in aux [] str

unwords :: [String] -> String
unwords [] = ""
unwords (lne:lnes) = lne ++ " " ++ (unwords lnes)
