--
data BinarySearchTree a 
    = Empty
    | Node {
        node::a, left::BinarySearchTree a, right::BinarySearchTree a
        }
    deriving (Show)


(..>) :: (Ord a) => a -> BinarySearchTree a -> BinarySearchTree a
d ..> Empty = Node d Empty Empty
d ..> Node n lh rh
    | d < n     = Node n (d ..> lh) rh 
    | otherwise = Node n lh (d ..> rh)

fromList :: (Ord a) => [a] -> BinarySearchTree a
fromList = foldr (..>) Empty

member :: (Ord a) => a -> BinarySearchTree a -> Bool
_ `member` Empty = False
d `member` Node n lh rh
    | d == n    = True
    | d <  n    = d `member` lh
    | otherwise = d `member` rh

--
main :: IO()
main = do
    let
        arr = [3, 1, 4, 1, 5, 9, 2] :: [Int]
        tree = fromList . reverse $ arr

    print $ tree
    -- Node {node = 3, 
    --     left = Node {node = 1, 
    --         left = Empty,
    --         right = Node {node = 1, 
    --             left = Empty, 
    --             right = Node {node = 2, 
    --                 left = Empty, 
    --                 right = Empty
    --             }
    --         }
    --     }, 
    --     right = Node {node = 4, 
    --         left = Empty, 
    --         right = Node {node = 5, 
    --             left = Empty, 
    --             right = Node {node = 9, 
    --                 left = Empty, 
    --                 right = Empty
    --             }
    --         }
    --     }
    -- }

    print $ 5 `member` tree     -- True
    print $ 7 `member` tree     -- False
