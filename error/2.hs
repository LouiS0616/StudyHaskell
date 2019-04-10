merge [] ys = ys
merge xs [] = xs

merge (x:xs) (y:ys)
    | x <= y    = x: merge    xs  (y:ys)
    | otherwise = y: merge (x:xs)    ys

--msort [e] = [e]
msort []  = []
msort xs  = merge (msort left) (msort right)
    where
        n     = (length xs) `div` 2
        left  = take n xs
        right = drop n xs


main = do
    print $ msort [1, 2, 3]
