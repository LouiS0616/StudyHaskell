import System.Random


randSmallChar = getStdRandom $ randomR ('a', 'z')

main = do
    r <- randSmallChar
    print r

    if r == 'z' 
        then print "END"
        else main 
