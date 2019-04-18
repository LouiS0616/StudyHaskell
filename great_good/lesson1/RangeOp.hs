--
_cycle' src [] = _cycle' src src
_cycle' src (x:xs) = x: _cycle' src xs

cycle' src = _cycle' src []

--
repeat' e = e: repeat' e

--
replicate' 0 _ = []
replicate' n e = e: replicate' (n-1) e
