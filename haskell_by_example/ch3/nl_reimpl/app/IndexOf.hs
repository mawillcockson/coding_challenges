module IndexOf (main, indexOf) where

main :: IO ()
main = do
    putStrLn ("found " ++ maybe "nothing" (\i -> "at " ++ show i) (indexOf 'b' "abc"))
    putStrLn ("found " ++ maybe "nothing" (\i -> "at " ++ show i) (indexOf 'd' "abc"))

indexOf :: Eq a => a -> [a] -> Maybe Integer
indexOf element xs = helper 0 element xs where
    helper :: Eq a => Integer -> a -> [a] -> Maybe Integer
    helper _ _ [] = Nothing
    helper index element (x:xs) = if element == x
        then Just index
        else helper (index + 1) element xs
