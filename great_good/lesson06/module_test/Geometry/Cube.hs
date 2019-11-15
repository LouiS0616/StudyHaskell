module Geometry.Cube (
    volume, area
    ) where


volume :: Float -> Float
volume = (^ (3::Int))

area :: Float -> Float
area = (6 *) . (^ (2::Int))
