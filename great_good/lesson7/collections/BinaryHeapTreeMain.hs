import Data.List
import BinaryHeapTree


main :: IO()
main = do
    let
        arr1 = [3, 1, 4, 1, 5, 9, 2, 6] :: [Int]
        arr2 = [2, 7, 1, 8]             :: [Int]
        arr3 = [1, 4, 1, 4, 2]          :: [Int]

    putStrLn $ makeSortLog arr1
    putStrLn $ makeSortLog arr2
    putStrLn $ makeSortLog arr3
    
    where
        makeSortLog arr = let dst = popAll . fromList $ arr
            in "   " ++ show arr ++ "\n"
            ++ "-> " ++ show dst ++ "\n"
            ++ "=> " ++ show (sort arr) ++ "\n"

