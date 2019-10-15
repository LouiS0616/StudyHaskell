import qualified Shape

main :: IO()
main = do
    print $ Shape.area (Shape.Circle 10)
    print $ Shape.area (Shape.Rectangle 10 20)

    print $ Shape.Square 10
