module MergeSort where

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

