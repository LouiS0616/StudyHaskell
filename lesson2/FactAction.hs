factAction 0 = return 1
factAction n
    | n > 0 = do
        n' <- factAction (n - 1)
        return $ n * n'

main = do
    print =<< factAction 5

