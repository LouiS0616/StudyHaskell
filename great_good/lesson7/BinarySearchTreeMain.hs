import BinarySearchTree


--
main :: IO()
main = do
    let
        arr = [3, 1, 4, 1, 5, 9, 2] :: [Int]
        tree = fromList . reverse $ arr

    print $ 5 `member` tree     -- True
    print $ 8 `member` tree     -- False

    let
        newTree = (delete 5) . (8 ..>) $ tree

    print $ 5 `member` newTree  -- False
    print $ 8 `member` newTree  -- True

    --
    print $ foldl (flip delete) newTree [2, 9, 1, 4, 1, 3, 5, 8]    -- Empty

