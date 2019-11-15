--
head' :: [a] -> a
head' []    = error "empty list"
head' (x:_) = x

tail' :: [a] -> [a]
tail' []     = []
tail' (_:xs) = xs

--
last1' :: [a] -> a
last1' xs = head (reverse xs)

last2' :: [a] -> a
last2' []     = error "empty list"
last2' [e]    = e
last2' (_:xs) = last2' xs

last3' :: [a] -> a
last3' xs = xs !! (length xs - 1)

--
init1' :: [a] -> [a]
init1' xs = reverse (tail (reverse xs))

init2' :: [a] -> [a]
init2' []     = []
init2' [e, _] = [e]
init2' (x:xs) = x: init2' xs

init3' :: [a] -> [a]
init3' xs = take (length xs - 1) xs

--
length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

--
null1' :: (Eq a) => [a] -> Bool
null1' xs = xs == []

null2' :: (Eq a) => [a] -> Bool
null2' xs = 0 == length xs

--
reverse' :: [a] -> [a]
reverse' []     = []
reverse' [e]    = [e]
reverse' (x:xs) = (reverse' xs) ++ [x]

--
take' :: Int -> [a] -> [a]
take' _ []      = []
take' n (x:xs)
    | n > 0     = x: take' (n-1) xs
    | otherwise = []

--
drop' :: Int -> [a] -> [a]
drop' _ []      = []
drop' n (x:xs)
    | n > 0     = drop' (n-1) xs
    | otherwise = x: xs

--
maximum' :: (Ord a) => [a] -> a
maximum' []       = error "empty list"
maximum' [e]      = e
maximum' (x:y:zs) = maximum' xs
    where
        xs = (max x y): zs

minimum' :: (Ord a) => [a] -> a
minimum' []       = error "empty list"
minimum' [e]      = e
minimum' (x:y:zs) = minimum' xs
    where
        xs = (min x y): zs

--
sum' :: (Num a) => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

product' :: (Num a) => [a] -> a
product' []     = 1
product' (x:xs) = x * product' xs

--
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
    | e == x    = True
    | otherwise = elem' e xs


--
rJust :: Int -> String -> String
rJust width str
    | width <= lenStr   = str
    | otherwise         = replicate (width - lenStr) ' ' ++ str
    where
        lenStr = length str

showWithTag :: (Show a) => Int -> String -> a -> String
showWithTag width tag dta
    | width <= tagLen + strLen  = tag ++ str
    | otherwise                 = tag ++ rJust (width - tagLen) str
    where
        str = show dta
        
        tagLen = length tag
        strLen = length str

showWithTag28 :: (Show a) => String -> a -> String
showWithTag28 = showWithTag 28

main :: IO()
main = do
    let
        lst = [3, 1, 4, 1, 5, 9, 2] :: [Int]
        putLine = putStrLn ""
   
    putStrLn $ showWithTag28 "raw: " lst
    putLine
    
    putStrLn $ showWithTag28 "head: "  (head  lst)
    putStrLn $ showWithTag28 "head': " (head' lst)
    putLine

    putStrLn $ showWithTag28 "tail: "  (tail  lst)
    putStrLn $ showWithTag28 "tail': " (tail' lst)
    putLine

    putStrLn $ showWithTag28 "last: "   (last   lst)
    putStrLn $ showWithTag28 "last1': " (last1' lst) 
    putStrLn $ showWithTag28 "last2': " (last2' lst) 
    putStrLn $ showWithTag28 "last3': " (last3' lst) 
    putLine

    putStrLn $ showWithTag28 "init: "   (init   lst)
    putStrLn $ showWithTag28 "init1': " (init1' lst)
    putStrLn $ showWithTag28 "init2': " (init2' lst)
    putStrLn $ showWithTag28 "init3': " (init3' lst)
    putLine

    putStrLn $ showWithTag28 "length: "  (length  lst)
    putStrLn $ showWithTag28 "length': " (length' lst)
    putLine

    putStrLn $ showWithTag28 "null: "   (null   lst)
    putStrLn $ showWithTag28 "null1': " (null1' lst)
    putStrLn $ showWithTag28 "null2': " (null2' lst)
    putLine

    putStrLn $ showWithTag28 "reverse: "  (reverse  lst)
    putStrLn $ showWithTag28 "reverse': " (reverse' lst)
    putLine

    putStrLn $ showWithTag28 "take: "  (take  3 lst)
    putStrLn $ showWithTag28 "take': " (take' 3 lst)
    putLine

    putStrLn $ showWithTag28 "drop: "  (drop  3 lst)
    putStrLn $ showWithTag28 "drop': " (drop' 3 lst)
    putLine

    putStrLn $ showWithTag28 "maximum: "  (maximum  lst)
    putStrLn $ showWithTag28 "maximum': " (maximum' lst)
    putLine

    putStrLn $ showWithTag28 "minimum: "  (minimum  lst)
    putStrLn $ showWithTag28 "minimum': " (minimum' lst)
    putLine

    putStrLn $ showWithTag28 "sum: "  (sum  lst)
    putStrLn $ showWithTag28 "sum': " (sum' lst)
    putLine
    
    putStrLn $ showWithTag28 "product: "  (product  lst)
    putStrLn $ showWithTag28 "product': " (product' lst)
    putLine

    putStrLn $ showWithTag28 "elem: "  (elem  5 lst)
    putStrLn $ showWithTag28 "elem': " (elem' 5 lst)
    putLine
