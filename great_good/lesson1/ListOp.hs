--
head' (x:_)  = x
tail' (_:xs) = xs

--
last1' xs = head (reverse xs)

last2' [e]    = e
last2' (x:xs) = last2' xs

last3' xs = xs !! (length xs - 1)

--
init1' xs = reverse (tail (reverse xs))

init2' [e, _] = [e]
init2' (x:xs) = x: init2' xs

init3' xs = take (length xs - 1) xs

--
length' []     = 0
length' (x:xs) = 1 + length' xs

--
null1' xs = xs == []
null2' xs = 0 == length xs

--
reverse' [e]    = [e]
reverse' (x:xs) = (reverse' xs) ++ [x]

--
take' _ [] = []
take' n (x:xs)
    | n > 0     = x: take' (n-1) xs
    | otherwise = []

--
drop' _ [] = []
drop' n (x:xs)
    | n > 0     = drop' (n-1) xs
    | otherwise = x: xs

--
maximum' [e] = e
maximum' (x:y:zs) = maximum' xs
    where
        xs = (max x y): zs

minimum' [e] = e
minimum' (x:y:zs) = minimum' xs
    where
        xs = (min x y): zs

--
sum' [e]    = e
sum' (x:xs) = x + sum' xs

product' [e]    = e
product' (x:xs) = x * product' xs

--
elem' _ [] = False
elem' e (x:xs)
    | e == x    = True
    | otherwise = elem' e xs
