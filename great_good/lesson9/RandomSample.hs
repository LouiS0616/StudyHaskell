--
import qualified System.Random as Random

--
choice :: (Random.RandomGen g) => [a] -> g -> (a, g)
choice lst gen = (lst !! i, newGen)
    where
        (i, newGen) = Random.randomR (0, length lst - 1) gen

choices :: (Random.RandomGen g) => [a] -> g -> [a]
choices lst gen = value: choices lst newGen
    where
        (value, newGen) = choice lst gen

--
main :: IO()
main = do
    let
        arr = [3, 1, 4, 1, 5, 9, 2] :: [Int]

    gen1 <- Random.getStdGen
    print $ take 10 $ choices arr gen1

    gen2 <- Random.newStdGen
    print $ take 10 $ choices "spam" gen2
