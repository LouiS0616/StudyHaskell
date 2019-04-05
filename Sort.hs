-- Insert Sort
insert x []     = [x]
insert x (y:ys)
    | x < y     = x : y : ys
    | otherwise = y : insert x ys

isort []        = []
isort (x:xs)    = insert x (isort xs)

-- Bubble Sort
isSorted (x:y:zs)
    | x > y     = False
    | zs == []  = x < y
    | otherwise = isSorted (y:zs)

bswap [x]   = [x]
bswap (x:y:zs)
    | x > y     = y : bswap (x:zs)
    | otherwise = x : bswap (y:zs)

bsort list
    | isSorted list = list
    | otherwise     = bsort $ bswap list

-- Marge Sort


main = do
    let list = [4, 6, 9, 8, 3, 5, 1, 7, 2]

    print $ isort list
    print $ bsort list
