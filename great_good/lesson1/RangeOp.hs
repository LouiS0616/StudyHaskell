--
_cycle' :: [a] -> [a] -> [a]
_cycle' src [] = _cycle' src src
_cycle' src (x:xs) = x: _cycle' src xs

cycle' :: [a] -> [a]
cycle' src = _cycle' src []

--
repeat' :: a -> [a]
repeat' e = e: repeat' e

--
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n e = e: replicate' (n-1) e


--
rJust :: Int -> String -> String
rJust width str
    | width <= lenStr   = str
    | otherwise         = replicate (width - lenStr) ' ' ++ str
    where
        lenStr = length str

showWithTag :: (Show a) => Int -> String -> a -> String
showWithTag width tag dta
    | width <= tagLen + strLen  = tag ++ str
    | otherwise                 = tag ++ rJust (width - tagLen) str
    where
        str = show dta

        tagLen = length tag
        strLen = length str

showWithTag32 :: (Show a) => String -> a -> String
showWithTag32 = showWithTag 32


main :: IO()
main = do
    let
        lst = [3, 1, 4] :: [Int]
        putLine = putStrLn ""

    putStrLn $ showWithTag32 "cycle: "   (take (10::Int) (cycle  lst))
    putStrLn $ showWithTag32 "cycle': "  (take (10::Int) (cycle' lst))
    putLine

    putStrLn $ showWithTag32 "repeat: "  (take (10::Int) (repeat  (3::Int)))
    putStrLn $ showWithTag32 "repeat': " (take (10::Int) (repeat' (3::Int)))
    putLine

    putStrLn $ showWithTag32 "replicate: "  (replicate  (7::Int) (4::Int))
    putStrLn $ showWithTag32 "replicate': " (replicate' (7::Int) (4::Int))
    putLine
