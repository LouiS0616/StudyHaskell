--
zipWith1' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith1' _ [] _ = []
zipWith1' _ _ [] = []
zipWith1' f (x:xs) (y:ys) = f x y : zipWith1' f xs ys


starMap :: (a -> b -> c) -> [(a, b)] -> [c]
starMap _ [] = []
starMap f (x:xs) = let (a, b) = x in f a b : starMap f xs

zipWith2' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith2' f xs ys = starMap f (zip xs ys)


--
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f a b = f b a

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x: filter' p xs
    | otherwise =    filter' p xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x       = []
    | otherwise = x: takeWhile' p xs

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p arr@(x:xs)
    | p x       = arr
    | otherwise = dropWhile' p xs

--
negatePredicate :: (a -> Bool) -> (a -> Bool)
negatePredicate p e = not (p e)

--
main :: IO()
main = do
    let
        arrP = [3, 1, 4, 1, 5, 9, 2] :: [Int]
        arrE = [2, 7, 1, 8]          :: [Int]

    print $ zipWith   (+) arrP arrE
    print $ zipWith1' (+) arrP arrE
    print $ zipWith2' (+) arrP arrE             -- [5,8,5,9]

    print $ [arrE, arrP]
    print $ flip  (++) arrP arrE
    print $ flip' (++) arrP arrE                -- [2,7,1,8,3,1,4,1,5,9,2]

    print $ map  (2*) arrP
    print $ map' (2*) arrP                      -- [6,2,8,2,10,18,4]

    print $ filter  odd arrP
    print $ filter' odd arrP                    -- [3,1,1,5,9]
    
    print $ takeWhile' (> 3) arrP               -- [3,1]
    print $ dropWhile' (> 3) arrP               -- [4,1,5,9,2]

    print $ map                 even  arrP
    print $ map (negatePredicate odd) arrP      -- [False,False,True,False,False,False,True]
