--
--
_enumerate n [e]    = [(n, e)]
_enumerate n (x:xs) = (n, x): _enumerate (n+1) xs

enumerate xs = _enumerate 0 xs
