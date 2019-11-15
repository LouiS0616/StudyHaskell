--
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

--
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x: acc) []


--
reverse1' :: [a] -> [a]
reverse1' = foldl (\acc x -> x: acc) []

reverse2' :: [a] -> [a]
reverse2' = foldl (flip (:)) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x acc -> if f x then x: acc else acc) []

--
foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' _ [] = error "bad argument"
foldl1' f (x:xs) = foldl f x xs

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1' max

last' :: [a] -> a
last' = foldr1 (\_ acc -> acc) 

and' :: [Bool] -> Bool
and' = foldr1 (&&)

--
main :: IO()
main = do
    let
        arr = [3, 1, 4, 1, 5, 9, 2] :: [Int]
        
    print $ sum' arr
    print $ product' arr
    
    print $ map' even arr
    print $ filter' even arr
    
    print $ reverse1' arr
    print $ reverse2' arr
    
    print $ maximum' arr
    print $ last' arr
    print $ and' (map (0 <) arr)
    print $ and' (map (1 <) arr)
