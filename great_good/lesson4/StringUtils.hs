{-# OPTIONS -Wall #-}

--
--Ž©K
readUntilCh :: Char -> String -> (String, String)

readUntilCh _ "" = (       "",   "")
readUntilCh dst (ch:chs)
    | ch == dst  = (       "",  chs)
    | otherwise  = (ch: block, rest)
    where
        (block, rest) = readUntilCh dst chs

--
split' :: String -> [String]

split' []  = []
split' src = block: split' rest
    where
        (block, rest) = readUntilSpace src
        
        readUntilSpace = readUntilCh ' '

--
removeSpace' :: String -> String

removeSpace' "" = ""
removeSpace' (ch:chs)
    | ch == ' ' =     removeSpace' chs
    | otherwise = ch: removeSpace' chs

