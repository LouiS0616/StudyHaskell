--
data Dict k v = Empty | Node k v (Dict k v) 

instance (Show k, Show v) => Show (Dict k v) where
    show Empty = "{}"
    show nodes = "{" ++ show' nodes ++ "}"
        where
            show' Empty = ""
            show' (Node k v nxt) = show k ++ ": " ++ show v ++ ", " ++ show' nxt


insert :: (Ord k) => Dict k v -> (k, v) -> Dict k v
insert Empty (key, value) = Node key value Empty
insert src@(Node k v nxt) kv@(key, value)
    | key < k   = Node key value src
    | otherwise = Node k v (insert nxt kv)

fromList :: (Ord k) => [(k, v)] -> Dict k v
fromList = foldl insert Empty


instance Functor (Dict k) where
    fmap _ Empty = Empty
    fmap f (Node k v nxt) = Node k (f v) (fmap f nxt)


--
main :: IO()
main = do
    let 
        dct :: Dict Int String
        dct = fromList [(3, "March"), (1, "January"), (4, "April")]

    print $ insert dct (2, "February")
    print $ fmap (++ " month") dct

