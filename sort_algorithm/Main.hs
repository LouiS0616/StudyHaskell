import System.Random (randomRIO)

import BubbleSort1
import BubbleSort2
import InsertSort
import MergeSort
import QuickSort
import SelectSort1
import SelectSort2

-- Ref: https://stackoverflow.com/questions/30740366/list-with-random-numbers-in-haskell
randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
    r  <- randomRIO (1,100)
    rs <- randomList (n-1)
    return (r:rs)

--
--
main = do
    list <- randomList 10
    print list

    print $ BubbleSort1.bsort list
    print $ BubbleSort2.bsort list
    print $             isort list
    print $             msort list
    print $             qsort list
    print $ SelectSort1.ssort list
    print $ SelectSort2.ssort list
