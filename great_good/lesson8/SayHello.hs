import System.IO

main :: IO()
main = do
    putStr "input your name: "
    hFlush stdout

    name <- getLine

    if null name
        then putStrLn "see you"
        else do
            putStrLn $ "hello, " ++ name
            main

