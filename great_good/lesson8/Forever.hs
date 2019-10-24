--
forever' :: IO() -> IO()
forever' action = do
    action
    forever' action

--
main :: IO()
main = forever' $ do
    putStrLn "hello"
    _ <- getLine
    return ()

