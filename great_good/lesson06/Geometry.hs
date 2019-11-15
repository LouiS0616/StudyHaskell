module Geometry (
    sphereVolume, sphereArea,
    cubeVolume, cubeArea,
    cuboidVolume, cuboidArea
    ) where



--
pow :: (Real a) => Int -> a -> a
pow = flip (^)

--
sphereVolume :: Float -> Float
sphereVolume = (4 / 3 *) . (pi *) . pow 3 

sphereArea :: Float -> Float
sphereArea = (4 *) . (pi *) . pow 2


cubeVolume :: Float -> Float
cubeVolume = pow 3

cubeArea :: Float -> Float
cubeArea = (6 *) . pow 2


cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = a * b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = 2 * (a*b + b*c + c*a)
