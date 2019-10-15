--
data Vector a = Vector a a
    deriving (Show, Eq, Ord)

(+++) :: (Num a) => Vector a -> Vector a -> Vector a
(Vector x1 y1) +++ (Vector x2 y2) = Vector (x1+x2) (y1+y2)

(***) :: (Num a) => Vector a -> Vector a -> a
(Vector x1 y1) *** (Vector x2 y2) = x1*x2 + y1*y2


--
main :: IO()
main = do
    let 
        vec1 = Vector 10 20 :: Vector Int
        vec2 = Vector 30 40 :: Vector Int

    print $ vec1 +++ vec2
    print $ vec1 *** vec2

    print $ vec1 `compare` vec2
    print $ (vec1 +++ Vector 20 20) == vec2 

