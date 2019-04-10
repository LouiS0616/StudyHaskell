module QuickSort where

--
--
-- Quick Sort
qsort []     = []
qsort (x:xs) = left ++ [axis] ++ right
    where
        axis  = x
        left  = qsort [e | e <- xs, e <  axis]
        right = qsort [e | e <- xs, e >= axis]

