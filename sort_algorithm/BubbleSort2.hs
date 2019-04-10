module BubbleSort2 where

--
--
-- Bubble Sort
isSorted (x:y:zs)
    | x > y     = False
    | zs == []  = x < y
    | otherwise = isSorted (y:zs)

-- ��ԏ����Ȑ���擪�Ɏ����Ă���
bswap [x]   = [x]
bswap (x:ys)
    | x < y     = x:y:zs
    | otherwise = y:x:zs
    where
        (y:zs) = bswap ys

-- 7shi���̎���
bsort [] = []
bsort xs = y : bsort ys
    where
        (y:ys) = bswap xs

