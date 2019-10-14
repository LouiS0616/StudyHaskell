--
findValue' :: (Eq a) => [(a, b)] -> a -> Maybe b
findValue' [] _ = Nothing
findValue' (item:items) key
    | k == key  = Just v
    | otherwise = findValue' items key
    where
        (k, v) = item

findKey' :: (Eq b) => [(a, b)] -> b -> Maybe a
findKey' [] _ = Nothing
findKey' (item:items) value
    | v == value    = Just k
    | otherwise     = findKey' items value
    where
        (k, v) = item

--
main :: IO()
main = do
    let
        dct = 
            [
                ( 1, "January"), ( 2, "February"), ( 3, "March"    ),
                ( 4, "April"  ), ( 5, "May"     ), ( 6, "June"     ),
                ( 7, "July"   ), ( 8, "August"  ), ( 9, "September"),
                (10, "October"), (11, "November"), (12, "December" )
            ] 
            :: [(Int, String)]

    print $ findValue' dct 6            -- Just "June"
    print $ findKey' dct "June"         -- Just 6
    print $ findKey' dct "Octorber"     -- Nothing
