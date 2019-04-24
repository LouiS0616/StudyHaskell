--
--
factoricalInt :: Int -> Int

factoricalInt 0 = 1
factoricalInt n = n * factoricalInt (n-1)

--
factoricalInteger :: Integer -> Integer

factoricalInteger 0 = 1
factoricalInteger n = n * factoricalInteger (n-1)

--
factoricalNum 0 = 1
factoricalNum n = n * factoricalNum (n-1)

--
main = do
    print $ factoricalInt     50   -- => オーバーフロー 
    print $ factoricalInteger 50   -- => 
    print $ factoricalNum     50   -- => 

