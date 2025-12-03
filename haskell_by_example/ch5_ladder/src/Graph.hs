module Graph where

type DiGraph a = [(a, [a])]

member :: Eq a => a -> [(a, b)] -> Bool
member _ [] = False
member element ((x, _) : xs)
    | x == element = True
    | otherwise = member element xs
