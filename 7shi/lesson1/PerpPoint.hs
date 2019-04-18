determinant (a, b, c, d) = a * d - b * c

perpPoint (p, q) (a, b, c) = (x, y)
    where
        x = (a * c + b * deter) / denom
        y = (b * c - a * deter) / denom
        deter = determinant (b, q, a, p) 
        denom = a * a + b * b

main = do
    print $ perpPoint (0, 2) (1, -1, 0)     -- => (1.0,1.0)
