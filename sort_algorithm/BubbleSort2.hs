module BubbleSort2 where

--
--
-- Bubble Sort
isSorted (x:y:zs)
    | x > y     = False
    | zs == []  = x < y
    | otherwise = isSorted (y:zs)

-- 一番小さな数を先頭に持ってくる
bswap [x]   = [x]
bswap (x:ys)
    | x < y     = x:y:zs
    | otherwise = y:x:zs
    where
        (y:zs) = bswap ys

-- 7shi氏の実装
bsort [] = []
bsort xs = y : bsort ys
    where
        (y:ys) = bswap xs

