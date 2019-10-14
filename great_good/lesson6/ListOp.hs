--
nub' :: (Eq a) => [a] -> [a]
nub' src = nub_' src []
    where
        nub_' [] _ = []
        nub_' (x:xs) ap
            | x `elem` ap   = nub_' xs ap
            | otherwise     = x: nub_' xs (x:ap)

--
splitWords' :: (Char -> Bool) -> String -> [String]
splitWords' _ "" = []
splitWords' p str = word: splitWords' p rest
    where
        word = takeWhile (not . p) str
        rest = dropWhile p . dropWhile (not . p) $ str

words' :: String -> [String]
words' = splitWords' (== ' ')        

--
groupBy' :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' p arr@(x:_) = block: groupBy' p rest
    where
        block = takeWhile (\y -> p x == p y) arr
        rest  = dropWhile (\y -> p x == p y) arr

group' :: (Eq a) => [a] -> [[a]]
group' = groupBy' id

qsort' :: (Ord a) => [a] -> [a]
qsort' [] = []
qsort' (x:xs) = qsort' left ++ [axis] ++ qsort' right
    where
        axis  = x
        left  = filter (<= x) xs
        right = filter (x  <) xs

--
startsWith' :: (Eq a) => [a] -> [a] -> Bool
startsWith' purpose frm = purpose == frmHead
    where
        frmHead = take (length purpose) frm

included' :: (Eq a) => [a] -> [a] -> Bool
included' _ [] = False
included' purpose frm@(_:xs)
    | startsWith' purpose frm   = True
    | otherwise                 = included' purpose xs

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' arr = arr: tails' (tail arr)

--
shiftOrd' :: (Enum a) => Int -> a -> a
shiftOrd' n src 
    | 0 <= n    = iterate succ src !!   n
    | otherwise = iterate pred src !! (-n)

caesarShift' :: (Enum a) => Int -> [a] -> [a]
caesarShift' 0 = id
caesarShift' n = map (shiftOrd' n)

--
main :: IO()
main = do
    let
        arr = [3, 1, 4, 1, 5, 9, 2] :: [Int]
        str = "Hi, this is a    spam."

    print $ nub' arr    -- [3,1,4,5,9,2]
    
    -- 単語を数える
    print $ words' str                                          -- ["Hi,","this","is","a","spam."]
    print $ let isAlpha = flip elem $ ['a'..'z'] ++ ['A'..'Z'] 
            in splitWords' (not . isAlpha) str                  -- ["Hi","this","is","a","spam"]

    print $ groupBy' (flip mod 3) arr   -- [[3],[1,4,1],[5],[9],[2]]
    print $ group' . qsort' $ arr       -- [[1,1],[2],[3],[4],[5],[9]]

    -- 干し草の山から針を探す
    print $ startsWith' "ab" "abcde"    -- True
    print $ startsWith' "ab" "cab"      -- False
    print $ included'   "ab" "abcde"    -- True
    print $ included'   "ab" "cab"      -- True

    print $ (tails' . take 5) arr       -- [[3,1,4,1,5],[1,4,1,5],[4,1,5],[1,5],[5],[]]
    
    -- シーザー暗号サラダ
    putStrLn $ caesarShift' (-1) "hoge"     -- gnfd
    putStrLn $ caesarShift'   0  "hoge"     -- hoge
    putStrLn $ caesarShift'   1  "hoge"     -- iphf

    print $ caesarShift' 10 arr             -- [13,11,14,11,15,19,12]

