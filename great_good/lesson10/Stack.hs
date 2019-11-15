--
module Stack where
--module Stack (
--    Stack(Empty), push, pop
--    ) where

--
import qualified Data.List as List

--
data Stack a = Empty | On a (Stack a)

instance (Show a) => Show (Stack a) where
    show stack = "[" ++ mid ++ "]"
        where
            mid :: String
            mid = (List.intercalate "," . map show . stackToList) stack


listToStack :: [a] -> Stack a
listToStack = foldr On Empty

stackToList :: Stack a -> [a]
stackToList Empty = []
stackToList (On e es) = e: stackToList es


push :: Stack a -> a -> Stack a
push = flip On

pop :: Stack a -> (a, Stack a)
pop Empty     = error "Stack is empty."
pop (On e es) = (e, es)

