{-# OPTIONS -Wall #-}

--
fibonacci :: Int -> Int

fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n
    | n < 0     = error "its negative"
    | otherwise = fibonacci (n-2) + fibonacci (n-1)


--
maximum' :: (Ord a) => [a] -> a

maximum' []     = error "empty"
maximum' [x]    = x
maximum' (x:xs) = max x (maximum' xs)


--
replicate' :: Int -> a -> [a]

replicate' 0 _ = []
replicate' n e
    | n < 0     = [] 
    | otherwise = e: replicate' (n-1) e


--
take' :: Int -> [a] -> [a]

take' _ []      = []
take' n (x:xs)
    | n <= 0    = []
    | otherwise = x: take' (n-1) xs


--
reverse' :: [a] -> [a]

reverse' []     = []
reverse' (x:xs) = (reverse' xs) ++ [x]


--
repeat' :: a -> [a]
repeat' x = x: repeat' x


--
zip' :: [a] -> [b] -> [(a, b)]

zip' []  _ = []
zip'  _ [] = []
zip' (x:xs) (y:ys)
    | empty' xs || empty' ys = (x, y): []
    | otherwise              = (x, y): zip' xs ys
    where
        --empty' lst = length lst == 0     –³ŒÀ’·ƒŠƒXƒg‚Ì’·‚³‚ð‘ª‚Á‚Ä‚µ‚Ü‚Á‚½...
        empty' [] = True
        empty' _  = False


zip2' :: [a] -> [b] -> [(a, b)]

zip2' (x:xs) (y:ys) = (x, y): zip2' xs ys
zip2'     _      _  = []


--
elem' :: (Eq a) => a -> [a] -> Bool

_ `elem'` []    = False
e `elem'` (x:xs)
    | e == x    = True
    | otherwise = e `elem'` xs


--
qsort' :: (Ord a) => [a] -> [a]

qsort' []     = []
qsort' (x:xs) = left ++ [pivot] ++ right
    where
        left  = qsort' [e | e <- xs, e <= pivot]
        pivot = x
        right = qsort' [e | e <- xs, e >  pivot]

