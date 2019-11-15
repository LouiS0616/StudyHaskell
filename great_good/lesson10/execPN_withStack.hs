--
import qualified Text.Read

import Stack



execPN_withStack' :: (Read a) => [String] -> Stack a -> Stack a

execPN_withStack' [] stack = stack

execPN_withStack' (op: ops) stack =
    case Text.Read.readMaybe op
    of Just value -> execPN_withStack' ops (push stack value)

execPN_withStack :: (Read a) => [String] -> Stack a
execPN_withStack ops = execPN_withStack' ops Stack.Empty


--
applyFunc :: (Num a) => (a -> a -> a) -> Stack a -> (a, Stack a)
applyFunc f stack = (f op1 op2, newStack2)
    where
        (op1, newStack1) = pop stack
        (op2, newStack2) = pop newStack1


--
main :: IO()
main = do
    let
        ops = ["3", "1"]
    
    print $ (execPN_withStack ops :: Stack Int)
