module BinarySearchTree (
    fromList, member, (..>), delete
    ) where


--
data BinarySearchTree a 
    = Empty
    | Node {
        node::a, left::BinarySearchTree a, right::BinarySearchTree a
        }
    deriving (Show, Eq)


isEmpty :: (Eq a) => BinarySearchTree a -> Bool
isEmpty = (== Empty)

isLeaf :: (Eq a) => BinarySearchTree a -> Bool
isLeaf Empty                = error "empty tree!"
isLeaf (Node _ Empty Empty) = True
isLeaf _                    = False


(..>) :: (Ord a) => a -> BinarySearchTree a -> BinarySearchTree a
d ..> Empty = Node d Empty Empty
d ..> Node n lh rh
    | d < n     = Node n (d ..> lh) rh 
    | otherwise = Node n lh (d ..> rh)

delete :: (Ord a) => a -> BinarySearchTree a -> BinarySearchTree a
_ `delete` Empty = Empty
d `delete` tree@(Node n lh rh)
    |     d < n = Node n (d `delete` lh) rh
    | n < d     = Node n lh (d `delete` rh)
    | isLeaf tree   = Empty
    | isEmpty lh    = rh
    | isEmpty rh    = lh
    | otherwise     = Node maxChildInLh newLh rh
    where
        (maxChildInLh, newLh) = popMaxNode . left $ tree


popMaxNode :: (Ord a) => BinarySearchTree a -> (a, BinarySearchTree a)
popMaxNode Empty             = error "failed to pop!"
popMaxNode (Node n lh Empty) = (n, lh)
popMaxNode (Node n lh rh)    = (poppedNode, Node n lh newTree)
    where
        (poppedNode, newTree) = popMaxNode rh


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
    print $ 8 `member` tree     -- False

    let
        newTree = (delete 5) . (8 ..>) $ tree

    print $ newTree
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
    --      }, 
    --      right = Node {node = 4, 
    --          left = Empty, 
    --          right = Node {node = 9, 
    --              left = Node {node = 8, 
    --                  left = Empty, 
    --                  right = Empty
    --              }, 
    --              right = Empty
    --          }
    --      }
    --  }

    print $ 5 `member` newTree
    print $ 8 `member` newTree
