import Shape

main :: IO()
main = do
    let
        circle = Circle {radius=10} :: Shape

    print $ area circle
    print $ radius circle

    print $ area (Rectangle {height=10, width=20})

    print $ Square 10

