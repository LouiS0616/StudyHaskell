import Data.Char

main :: IO()
main = do
    content <- getContents
    putStrLn $ map toUpper content
