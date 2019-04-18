-- https://qiita.com/7shi/items/145f1234f8ec2af923ef#%E7%B7%B4%E7%BF%92-8

isRightTriangle a b c
    = a^2 + b^2 == c^2 

isOrdered []  = True
isOrdered [_] = True
isOrdered (x:y:zs)
    | x > y     = False
    | otherwise = isOrdered (y:zs)

main = do
    print [
        (a, b, c) | a <- [1..20], b <- [1..20], c <- [1..20], 
        isOrdered [a, b, c],
        isRightTriangle a b c
        ]

    print [
        (a, b, c) | a <- [1..20], b <- [a..20], c <- [b..20],
        isRightTriangle a b c
        ]
