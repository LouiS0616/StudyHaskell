module Geometry.Sphere (
    volume, area
    ) where


volume :: Float -> Float
volume = (4/3 *) . (pi *) . (^ (3::Int))

area :: Float -> Float
area = (4 *) . (pi *) . (^ (2::Int))
