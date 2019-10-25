--
fizzbuzz :: Int -> String
fizzbuzz n
    | n `mod` 15 == 0   = "fizzbuzz"
    | n `mod`  3 == 0   = "fizz"
    | n `mod`  5 == 0   = "buzz"
    | otherwise         = show n

--
main :: IO ()
main = do
    input <- getLine
    let
        n = read input

    mapM_ (putStrLn . fizzbuzz) [1..n] 

