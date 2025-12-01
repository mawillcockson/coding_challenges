module TasteOfMonad (main) where

-- read this:
-- https://academy.fpblock.com/haskell/tutorial/operators/

import System.Environment (getArgs)
main :: IO ()
main = getArgs >>= putStrLn . show
