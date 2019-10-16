module BinaryHeapTree (
        fromList
    ) where

--
data BinaryHeapTree a
    = Empty
    | Node { 
            node::a, 
            left::BinaryHeapTree a, right::BinaryHeapTree a
        }   
    deriving (Eq)

instance (Show a) => Show (BinaryHeapTree a) where
    show = toStr 1


--
isSaturated :: BinaryHeapTree a -> Bool
isSaturated Empty = True
isSaturated (Node _ lh rh)
    = isSaturated lh && isSaturated rh && height lh == height rh

height :: BinaryHeapTree a -> Int
height Empty          = 0
height (Node _ lh rh) = 1 + max (height lh) (height rh)

lastNode :: (Eq a) => BinaryHeapTree a -> BinaryHeapTree a
lastNode Empty = error "empty tree!"
lastNode tree@(Node _ lh rh)
    | lh == Empty            = tree
    | height lh == height rh = lastNode rh
    | otherwise              = lastNode lh

toListBreadthFirst :: BinaryHeapTree a -> [a]
toListBreadthFirst tree = withQueue [tree]
    where
        withQueue :: [BinaryHeapTree a] -> [a]
        withQueue []         = []
        withQueue (Empty:xs) = withQueue xs
        withQueue (    t:xs) = node t: withQueue (xs ++ [left t, right t]) 

--
(..>) :: (Ord a) => a -> BinaryHeapTree a -> BinaryHeapTree a
(..>) = putAtLast 
--d ..> tree = upHeap (lastNode (putAtLast d tree))

--pushAll :: (Ord a) => [a] -> BinaryHeapTree a -> BinaryHeapTree a
--pushAll xs tree = foldl (flip (..>)) tree xs

fromList :: (Ord a) => [a] -> BinaryHeapTree a
fromList = foldl (flip (..>)) Empty

--
putAtLast :: (Eq a) => a -> BinaryHeapTree a -> BinaryHeapTree a
putAtLast d Empty 
                        = Node d Empty Empty
putAtLast d tree@(Node n lh rh) 
    | lh == Empty       = Node n (Node d Empty Empty)               Empty
    | rh == Empty       = Node n                  lh  (Node d Empty Empty)
    | isSaturated tree  = Node n     (putAtLast d lh)                  rh 
    | isSaturated lh    = Node n                  lh      (putAtLast d rh)
    | otherwise         = Node n     (putAtLast d lh)                  rh 

--upHeap :: (Ord a) => BinaryHeapTree a -> BinaryHeapTree a
--upHeap Empty = Empty
--upHeap tree@(Node _ Empty _ _) = tree
--upHeap tree@(Node n p lh rh)
--    | n < node p = upHeap newNode 
--    | otherwise  = upHeap p
--    where
--        untreeSide = if left p == tree then right p else left p
--        
--        root = Node n (parent p) Empty Empty
--        elems = [node p]
--             ++ (toListBreadthFirst untreeSide)
--             ++ (toListBreadthFirst lh) ++ (toListBreadthFirst rh)
--       
--        newNode = pushAll elems root

--
toStr :: (Show a) => Int -> BinaryHeapTree a -> String
toStr _ Empty = "Empty"
toStr _ (Node n Empty Empty) 
    = "Leaf { node = " ++ show n ++ "}"
toStr d (Node n lh rh) 
    = "Node { node = " ++ show n ++ ",\n"
    ++ indent d ++ "left  = " ++ toStr (d+1) lh ++ ",\n" 
    ++ indent d ++ "right = " ++ toStr (d+1) rh ++  "\n"
    ++ indent (d-1) ++ "}"

indent :: Int -> String
indent n = join "" (replicate n "    ")
    where
        join :: String -> [String] -> String
        join _ [ ]        = ""
        join _ [x]        = x
        join sep (x:y:zs) = x ++ sep ++ join sep (y:zs) 
