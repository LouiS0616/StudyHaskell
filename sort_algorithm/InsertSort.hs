module InsertSort where

--
--
-- Insert Sort
insert x []     = [x]
insert x (y:ys)
    | x < y     = x : y : ys
    | otherwise = y : insert x ys

isort []     = []
isort (x:xs) = insert x (isort xs)
