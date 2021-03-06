{-# OPTIONS -Wall #-}

-- where"節"を使った場合
abs1 :: Int -> Int
abs1 n
    | positive  =  n
    | otherwise = -n
    where
        positive = n > 0

-- let"式"を使った場合
abs2 :: Int -> Int
abs2 n 
    | let positive = n > 0 in positive =  n
    | otherwise                        = -n 
