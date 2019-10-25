import Data.Char

--
interact' :: (String -> String) -> IO()
interact' f = do
    line <- getContents
    putStrLn $ f line 

--
main :: IO()
main = do
    --interact  $ map toUpper
    interact' $ map toUpper
