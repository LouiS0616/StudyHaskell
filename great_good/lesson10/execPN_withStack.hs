--
import Stack



execPN_withStack' :: (Read a) => [String] -> Stack a -> (a, Stack a)
execPN_withStack' (o: os) stack = (read o, stack)


--
main :: IO()
main = do
    let
        op = ["3", "1", "+"]
    
    print $ execPN_withStack' op (Stack.Empty :: Stack Int)

