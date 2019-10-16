import BinaryHeapTree


main :: IO()
main = do
    let
        arr = [3, 1, 4, 1, 5, 9, 2, 6] :: [Int]
        tree = fromList arr

    print $ tree

