module Integers (example) where

example :: IO ()
example = do
    putStrLn $ "f(-100): " ++ (show $ f (-100))
    putStrLn $ "f(0): " ++ (show $ f 0)
    putStrLn $ "f(1): " ++ (show $ f 1)
    putStrLn $ "f(2): " ++ (show $ f 2)
    putStrLn $ "f(3): " ++ (show $ f 3)
    putStrLn $ "f(100): " ++ (show $ f 100)
    putStrLn $ "f(4): " ++ (show $ f 4)

f :: Integer -> Integer
f 1 = 5
f 3 = 10
f x | x <= 0 = -1
    | x > 4 = 20
f 4 = 20
