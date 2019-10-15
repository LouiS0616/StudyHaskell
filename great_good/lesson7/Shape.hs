module Shape (
    Shape (..),
    area
    ) where


data Shape = Circle    {radius :: Float}
           | Square    {side   :: Float}
           | Rectangle {width :: Float, height :: Float}
    deriving Show

area :: Shape -> Float
area (Circle r)      = 2 * pi * r * r
area (Square s)      = s * s
area (Rectangle w h) = w * h

