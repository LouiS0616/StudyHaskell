import Debug.Trace

--
-- Select Sort
_select prov [] = (prov, [])
_select prov (x:xs)
    | x < prov  = (minimum, (prov:list))
    | otherwise = (minimum, (   x:list))
    where
        (minimum, list) = _select newProv xs
        newProv         = if x < prov then x else prov
    
--select [x]    = (      x,   [])
select (x:xs) = (minimum, list)
    where
        (minimum, list) = _select x xs

ssort [x]  = [x]
ssort list = e: ssort newList
    where 
        (e, newList) = select list

--
-- Insert Sort
insert e [] = [e]
insert e (x:ys)
    | e <= x    = e: x: ys
    | otherwise = x: insert e ys 

isort []     = []
isort (x:ys) = insert x (isort ys)

--
-- Merge Sort
merge [] ys = ys
merge xs [] = xs

merge (x:xs) (y:ys)
    | x <= y    = x: merge    xs  (y:ys)
    | otherwise = y: merge (x:xs)    ys

msort [e] = [e]
msort xs  = merge (msort left) (msort right) 
    where
        n     = (length xs) `div` 2
        left  = take n xs
        right = drop n xs

--
-- Quick Sort
qsort []     = []
qsort (x:ys) = (qsort left) ++ [axis] ++ (qsort right)
    where
        left  = [e | e <- ys, e <= axis]
        axis  = x
        right = [e | e <- ys, e >  axis]

main = do
    let list = [9, 2, 5, 3, 6, 7]
    print list

    print $ isort list
    print $ qsort list
    print $ msort list
    print $ ssort list
