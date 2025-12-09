module Main (main) where

import qualified System.Environment

main :: IO ()
main = do
    name <- System.Environment.getProgName
    putStrLn name
