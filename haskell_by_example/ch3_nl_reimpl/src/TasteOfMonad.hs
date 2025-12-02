module TasteOfMonad (example) where

-- read this:
-- https://academy.fpblock.com/haskell/tutorial/operators/

import System.Environment (getArgs)
example :: IO ()
example = getArgs >>= putStrLn . show
