import Data.Char

selectRotatively list idx
    = list !! (mod idx $ length list)

rotate n ch
    | isUpper ch = selectRotatively ['A'..'Z'] ((ord ch) - (ord 'A') + n)
    | isLower ch = selectRotatively ['a'..'z'] ((ord ch) - (ord 'a') + n)
    | otherwise  = ch

rot13 ""        = ""
rot13 (ch:chs)  = (rotate 13 ch) : rot13 chs

main = do
    print $ rot13 "Hello, world!"
