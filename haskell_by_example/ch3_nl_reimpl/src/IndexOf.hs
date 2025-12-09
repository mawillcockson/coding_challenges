module IndexOf (example, indexOf) where

example :: IO ()
example = do
    putStrLn ("found " ++ maybe "nothing" (\i -> "at " ++ show i) (indexOf 'b' "abc"))
    putStrLn ("found " ++ maybe "nothing" (\i -> "at " ++ show i) (indexOf 'd' "abc"))

indexOf :: (Eq a) => a -> [a] -> Maybe Integer
indexOf element somethings = helper 0 element somethings
  where
    helper :: (Eq a) => Integer -> a -> [a] -> Maybe Integer
    helper _ _ [] = Nothing
    helper index elmnt (x : xs) =
        if elmnt == x
            then Just index
            else helper (index + 1) elmnt xs
