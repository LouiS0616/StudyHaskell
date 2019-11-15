--
data QuadraticEq t = QuadraticEq t t t

instance (Show t) => Show (QuadraticEq t) where
    show (QuadraticEq a b c) = show a ++ "x^2 + " 
                            ++ show b ++ "x + " 
                            ++ show c 
                            ++ " = 0"


data Roots = NoRoot 
           | Roots Double Double

instance Show Roots where
    show NoRoot = "No root"
    show (Roots x1 x2)
        | x1 == x2  = "Duplicate root: x = " ++ show x1
        | otherwise = "Roots: x = " ++ show x1 ++ ", " ++ show x2


--
solve :: (Integral t) => QuadraticEq t -> Roots
solve (QuadraticEq a b c)
    | 0 <= d    = Roots x1 x2
    | otherwise = NoRoot
    where
        (*/) :: (Integral t) => t -> t -> Double
        n */ m = fromIntegral n / fromIntegral m

        d = b * b - 4 * a * c
        x1 = (- b + d) */ (2 * a) :: Double
        x2 = (- b - d) */ (2 * a) :: Double


--
instance Functor QuadraticEq where
    fmap f (QuadraticEq a b c) = QuadraticEq (f a) (f b) (f c)

--
main :: IO()
main = do
    let
        eq1 = QuadraticEq 1 3 2    :: QuadraticEq Int  -- x = -1, x = -2
        eq2 = QuadraticEq 1 (-4) 4 :: QuadraticEq Int  -- x = 2
        eq3 = QuadraticEq 1 2 3    :: QuadraticEq Int  -- no real root
     -- eq4 = QuadraticEq 0 1 1    :: QuadraticEq Int  -- cannot be interpret as quandratic equation

    putStrLn $ showSolve eq1
    putStrLn $ showSolve eq2
    putStrLn $ showSolve eq3

    print $ fmap (*2) eq1
    print $ fmap (*3) eq1
    print $ fmap (*4) eq1

    where
        showSolve :: (Integral t, Show t) => QuadraticEq t -> String
        showSolve eq = "   " ++ show eq         ++ "\n" 
                    ++ "-> " ++ show (solve eq) ++ "\n"

