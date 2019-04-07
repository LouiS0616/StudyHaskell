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

-- ˆê”Ô¬‚³‚È”‚ðæ“ª‚ÉŽ‚Á‚Ä‚­‚é
bswap [x]   = [x]
bswap (x:ys)
    | x < y     = x:y:zs
    | otherwise = y:x:zs
    where
        (y:zs) = bswap ys

-- “ÆŽ©ŽÀ‘•
bsort1 list
    | isSorted list = list
    | otherwise     = bsort1 $ bswap list

-- 7shiŽ‚ÌŽÀ‘•
bsort2 [] = []
bsort2 xs = y : bsort2 ys
    where
        (y:ys) = bswap xs

--
--
-- Merge Sort
merge [] ys = ys
merge xs [] = xs

merge (x:xs) (y:ys)
    | x < y     = x : merge    xs (y:ys)
    | otherwise = y : merge (x:xs)   ys

msort [x] = [x]
msort xs  = merge left right
    where
        left  = msort $ take n xs
        right = msort $ drop n xs
        n = (length xs) `div` 2         -- ®”œŽZ‚Ì‚½‚ß‚Ìdiv

--
--
-- Quick Sort
qsort []     = []
qsort (x:xs) = left ++ [axis] ++ right
    where
        axis  = x
        left  = qsort [e | e <- xs, e <  axis]
        right = qsort [e | e <- xs, e >= axis]

--
--
main = do
    let list = [4, 6, 9, 8, 3, 5, 1, 7, 2]

    print $ isort  list
    print $ bsort1 list
    print $ bsort2 list
    print $ msort  list
    print $ qsort  list
