import qualified Geometry

main :: IO()
main = do
    print $ Geometry.sphereVolume 10
    print $ Geometry.sphereArea   10
    print $ Geometry.cubeVolume   10
    print $ Geometry.cubeArea     10
    print $ Geometry.cuboidVolume 10 20 30 
    print $ Geometry.cuboidArea   10 20 30

    --print $ Geometry.pow 3 2
    --error: Module Geometry does not export pow.
