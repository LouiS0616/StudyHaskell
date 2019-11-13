--
split' :: Char -> String -> [String]
split' _ "" = []
split' ch src = takeWhile (/= ch) src: split' ch rest
    where
        rest = case dropWhile (/= ch) src of
            ""     -> ""
            (_: r) -> r

join' :: Char -> [String] -> String
join' ch = foldl1 (\acc str -> acc ++ [ch] ++ str)

--
words' :: String -> [String]
words' = split' ' '

unwords' :: [String] -> String
unwords' = join' ' '

lines' :: String -> [String]
lines' = split' '\n'

unlines' :: [String] -> String
unlines' = join' '\n'


--
accumulate' :: (Num a) => [a] -> [a]
accumulate' = scanl1 (+)

--
main :: IO()
main = do
    let
        str1 = "spam ham egg"
        str2 = "spam\nham\negg\n"

        arr = [3, 1, 4, 1, 5, 9, 2] :: [Int]

    print $ unwords' $ words' str1
    print $ unlines' $ lines' str2

    print $ accumulate' arr
