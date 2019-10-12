--
nub' :: (Eq a) => [a] -> [a]
nub' src = nub_' src []
    where
        nub_' [] _ = []
        nub_' (x:xs) ap
            | x `elem` ap   = nub_' xs ap
            | otherwise     = x: nub_' xs (x:ap)

splitWords' :: (Char -> Bool) -> String -> [String]
splitWords' _ "" = []
splitWords' p str = word: splitWords' p rest
    where
        word = takeWhile (not . p) str
        rest = dropWhile p $ dropWhile (not . p) str

words' :: String -> [String]
words' = splitWords' (== ' ')        

--
main :: IO()
main = do
    let
        arr = [3, 1, 4, 1, 5, 9, 2] :: [Int]
        str = "Hi, this is a    spam."

    print $ nub' arr
    
    print $ words' str
    print $ splitWords' (not . isAlpha) str
        where
            isAlpha = flip elem $ ['a'..'z'] ++ ['A'..'Z']
