length' []      = 0
length' (_:es)  = 1 + length' es

sum' []     = 0
sum' (e:es) = e + sum' es

product' []     = 1
product' (e:es) = e * product' es

take' n _ | n <= 0  = [] 
take' _ []          = []
take' n (e:es)      = e: take' (n-1) es

drop' n lst | n <= 0    = lst
drop' _ []              = []
drop' n (e:es)          = drop' (n-1) es

reverse' []     = []
reverse' (e:es) = (reverse' es) ++ [e]

fact 0  = 1
fact n  = product [1..n]

main = do
    print $ length' [1, 2, 3]
    print $ length' []

    print $ sum' [1..5]
    print $ sum' []
    
    print $ product' [1..5]
    print $ product' []

    print $ take'   2  [1, 2, 3]
    print $ take'   4  [1, 2, 3]
    print $ take' (-1) [1, 2, 3]

    print $ drop'   2  [1, 2, 3]
    print $ drop'   4  [1, 2, 3]
    print $ drop' (-1) [1, 2, 3]

    print $ reverse' [1, 2, 3]

    print $ fact 5
