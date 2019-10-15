import qualified Geometry.Sphere
import qualified Geometry.Cube
import qualified Geometry.Cuboid


main :: IO()
main = do
    print $ Geometry.Sphere.volume 10
    print $ Geometry.Sphere.area   10
    print $ Geometry.Cube.volume   10
    print $ Geometry.Cube.area     10
    print $ Geometry.Cuboid.volume 10 20 30
    print $ Geometry.Cuboid.area   10 20 30

