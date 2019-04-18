--
--
_enumerate1 n [e]    = [(n, e)]
_enumerate1 n (x:xs) = (n, x): _enumerate1 (n+1) xs

enumerate1 xs = _enumerate1 0 xs

--
enumerate2 xs = zip [0..] xs


--
--
take1' n xs = [
    e | (i, e) <- enumerate2 xs, i < n]

take2' n (x:xs)
    | n > 1     = x: take2' (n-1) xs
    | otherwise = [x]
