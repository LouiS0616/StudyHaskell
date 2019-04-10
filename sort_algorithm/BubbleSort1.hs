module BubbleSort1 where

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

-- �Ǝ�����
bsort list
    | isSorted list = list
    | otherwise     = bsort $ bswap list

