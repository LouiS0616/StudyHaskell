import qualified Data.Map.Strict as Map


--
main :: IO()
main = do
    let
        monthToName :: Map.Map Int String
        monthToName = Map.fromList $ [
                ( 1, "January"), ( 2, "Feburuary"), ( 3, "March"    ),
                ( 4, "April"  ), ( 5, "May"      ), ( 6, "June"     ),
                ( 7, "July"   ), ( 8, "August"   ), ( 9, "September"),
                (10, "October"), (11, "November" ), (12, "December" )
            ]

        getMonthName :: Int -> Maybe String
        getMonthName = flip Map.lookup $ monthToName 


    print $ getMonthName 6
    print $ getMonthName 16
