-- ‚±‚Á‚¿‚ÍƒGƒ‰[
func1 :: [a] -> a
func1 [e] = 0 
--    ? No instance for (Num a) arising from ther literal ef
--      Possible fix:
--        add (Num a) to the context of
--          the type signature for:
--            func1 :: forall a. [a] -> a
--    ? In the expression: 3
--      In an equation for unc1 func1 [e] = 3
--  |
--  | func1 [e] = 3
--  |            


-- ‚±‚Á‚¿‚Í‚Æ‚¨‚é
func2 :: [Int] -> Int
func2 [e] = 0
