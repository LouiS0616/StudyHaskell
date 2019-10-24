--
putStr' :: String -> IO()
putStr' [] = return ()
putStr' (x:xs) = do
    putChar x
    putStr' xs

putStrLn' :: String -> IO()
putStrLn' str = do
    putStr' str
    putChar '\n'

--
when' :: Bool -> IO() -> IO()
when' tf action = if tf then action else return ()

--
sequence' :: [IO()] -> IO()
sequence' [] = return ()
sequence' (x:xs) = do
    x
    sequence' xs

mapM' :: (a -> IO()) -> [a] -> IO [()]
mapM' f = sequence . map f

mapM_' :: (a -> IO()) -> [a] -> IO()
mapM_' f lst = do
    _ <- sequence $ map f lst
    return ()

--
forM' :: [a] -> (a -> IO()) -> IO()
forM' [] _ = return ()
forM' (x:xs) faction = do
    faction x
    forM' xs faction


--
main :: IO()
main = do
    putStrLn' "spam ham egg"
    sequence' [putStrLn "spam", putStrLn "ham", putStrLn "egg"]

    _ <- mapM'  print ([1, 2, 3] :: [Int])
    mapM_' print ([1, 2, 3] :: [Int])
    
    forM' ([1, 2, 3] :: [Int]) $ \e -> do
        print e

    sayYes


sayYes :: IO()
sayYes = do
    putStrLn "say yes"
    input <- getLine

    when' (input /= "yes") $ do
        sayYes

