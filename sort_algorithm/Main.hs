
--
--
main = do
    let list = [4, 6, 9, 8, 3, 5, 1, 7, 2]

    print $ isort  list
    print $ bsort1 list
    print $ bsort2 list
    print $ msort  list
    print $ qsort  list
