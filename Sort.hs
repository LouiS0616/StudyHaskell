--
--
-- Insert Sort
insert x []     = [x]
insert x (y:ys)
    | x < y     = x : y : ys
    | otherwise = y : insert x ys

isort []     = []
isort (x:xs) = insert x (isort xs)

--
--
-- Bubble Sort
isSorted (x:y:zs)
    | x > y     = False
    | zs == []  = x < y
    | otherwise = isSorted (y:zs)

-- ˆê”Ô¬‚³‚È”‚ğæ“ª‚É‚Á‚Ä‚­‚é
bswap [x]   = [x]
bswap (x:ys)
    | x < y     = x:y:zs
    | otherwise = y:x:zs
    where
        (y:zs) = bswap ys

-- “Æ©À‘•
bsort1 list
    | isSorted list = list
    | otherwise     = bsort1 $ bswap list

-- 7shi‚ÌÀ‘•
bsort2 [] = []
bsort2 xs = y : bsort2 ys
    where
        (y:ys) = bswap xs

--
--
-- Marge Sort


main = do
    let list = [4, 6, 9, 8, 3, 5, 1, 7, 2]

    print $ isort  list
    print $ bsort1 list
    print $ bsort2 list
