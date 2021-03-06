module SelectSort2 where

--
-- 二番目に書いた、分割統治を意識した方法
minInt [x]    = x
minInt (x:xs) = min x (minInt xs)

remove e (x:xs)
    | e == x    = xs
    | otherwise = x: remove e xs

ssort [] = []
ssort xs = e: ssort rest
    where
        e    = minInt(xs)
        rest = remove e xs

