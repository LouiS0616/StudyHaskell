--
_cycle1' :: [a] -> [a] -> [a]
_cycle1' src []     = _cycle1' src src
_cycle1' src (x:xs) = x: _cycle1' src xs

cycle1' :: [a] -> [a]
cycle1' src = _cycle1' src []

cycle2' :: [a] -> [a]
cycle2' lst = concat (lst: [cycle2' lst])

--
repeat' :: a -> [a]
repeat' e = e: repeat' e

--
replicate' :: Int -> a -> [a]
replicate' n e
    | n  < 0    = error "negative value"
    | n == 0    = []
    | otherwise = e: replicate' (n-1) e


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

    putStrLn $ showWithTag32 "cycle: "    (take (10::Int) (cycle   lst))
    putStrLn $ showWithTag32 "cycle1': "  (take (10::Int) (cycle1' lst))
    putStrLn $ showWithTag32 "cycle2': "  (take (10::Int) (cycle2' lst))
    putLine

    putStrLn $ showWithTag32 "repeat: "  (take (10::Int) (repeat  (3::Int)))
    putStrLn $ showWithTag32 "repeat': " (take (10::Int) (repeat' (3::Int)))
    putLine

    putStrLn $ showWithTag32 "replicate: "  (replicate  (7::Int) (4::Int))
    putStrLn $ showWithTag32 "replicate': " (replicate' (7::Int) (4::Int))
    putLine
