{-# OPTIONS -Wall #-}

-- where"��"���g�����ꍇ
abs1 :: Int -> Int
abs1 n
    | positive  =  n
    | otherwise = -n
    where
        positive = n > 0

-- let"��"���g�����ꍇ
abs2 :: Int -> Int
abs2 n 
    | let positive = n > 0 in positive =  n
    | otherwise                        = -n 
