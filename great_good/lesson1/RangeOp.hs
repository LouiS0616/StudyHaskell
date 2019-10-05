--
_cycle' src [] = _cycle' src src
_cycle' src (x:xs) = x: _cycle' src xs

cycle' src = _cycle' src []

--
repeat' e = e: repeat' e

--
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

    putStrLn $ showWithTag32 "cycle: "   (take 10 (cycle  lst))
    putStrLn $ showWithTag32 "cycle': "  (take 10 (cycle' lst))
    putLine

    putStrLn $ showWithTag32 "repeat: "  (take 10 (repeat  3))
    putStrLn $ showWithTag32 "repeat': " (take 10 (repeat' 3))
    putLine

    putStrLn $ showWithTag32 "replicate: "  (replicate  7 4)
    putStrLn $ showWithTag32 "replicate': " (replicate' 7 4)
    putLine
