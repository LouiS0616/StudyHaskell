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


data Direction = LeftDir | RightDir deriving (Eq, Show)


--
isSaturated :: BinaryHeapTree a -> Bool
isSaturated Empty = True
isSaturated (Node _ lh rh)
    = isSaturated lh && isSaturated rh && height lh == height rh

height :: BinaryHeapTree a -> Int
height Empty          = 0
height (Node _ lh rh) = 1 + max (height lh) (height rh)

directionsTowardLastNode :: (Eq a) => BinaryHeapTree a -> [Direction]
directionsTowardLastNode Empty = []
directionsTowardLastNode (Node _ lh rh)
    | lh == Empty            = []
    | height lh == height rh = RightDir: directionsTowardLastNode rh
    | otherwise              = LeftDir : directionsTowardLastNode lh


--
(..>) :: (Ord a) => a -> BinaryHeapTree a -> BinaryHeapTree a
d ..> tree = upHeap newTree (directionsTowardLastNode newTree)
    where
        newTree = putAtLast d tree

fromList :: (Ord a) => [a] -> BinaryHeapTree a
fromList = foldl (flip (..>)) Empty


--
putAtLast :: (Eq a) => a -> BinaryHeapTree a -> BinaryHeapTree a
putAtLast d Empty       = Node d Empty Empty
putAtLast d tree@(Node n lh rh) 
    | lh == Empty       = Node n (Node d Empty Empty)               Empty
    | rh == Empty       = Node n                  lh  (Node d Empty Empty)
    | isSaturated tree  = Node n     (putAtLast d lh)                  rh 
    | isSaturated lh    = Node n                  lh      (putAtLast d rh)
    | otherwise         = Node n     (putAtLast d lh)                  rh 

upHeap :: (Ord a) => BinaryHeapTree a -> [Direction] -> BinaryHeapTree a
upHeap tree@(Node _ Empty Empty) [] = tree 
upHeap (Node n lh rh) (LeftDir:ds)
    | n <= node lh  = Node        n  (upHeap    lh ds) rh
    | otherwise     = Node (node lh) (upHeap newLh ds) rh where newLh = Node n (left lh) (right lh)
upHeap (Node n lh rh) (RightDir:ds)
    | n <= node rh  = Node        n  lh (upHeap    rh ds)
    | otherwise     = Node (node rh) lh (upHeap newRh ds) where newRh = Node n (left rh) (right rh)
upHeap _ _ = error "illegal op"


--
toStr :: (Show a) => Int -> BinaryHeapTree a -> String
toStr _ Empty = "Empty"
toStr _ (Node n Empty Empty) 
    = "Leaf { node = " ++ show n ++ " }"
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
