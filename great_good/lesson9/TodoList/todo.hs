import qualified Control.Exception  as Exception
import qualified Control.Monad      as Monad

import qualified System.Directory   as Dir
import qualified System.Environment as Env
import qualified System.Exit        as Exit
import qualified System.IO          as SysIO


--
enumerate :: Int -> [a] -> [(Int, a)]
enumerate n = zip [n..]

popAt :: Int -> [a] -> (a, [a])
popAt n src = (src !! n, left ++ right)
    where
        left  = take  n    src
        right = drop (n+1) src
    

--
dispatch :: String -> [String] -> IO ()
dispatch "add"    args = add    args
dispatch "bump"   args = bump   args
dispatch "remove" args = remove args
dispatch "view"   args = view   args
dispatch _        _    = help


--
main :: IO ()
main = do
    --
    rawArgs <- Env.getArgs
    Monad.when (length rawArgs < 2) $ do
        help
        Exit.exitFailure
            
    --
    let 
        (mode: args) = rawArgs
   
    dispatch mode args


--
help :: IO ()
help = putStrLn $ unlines [
    "Usage:",
    "- prog.exe help",
    "- prog.exe add filename item1 item2 ...",
    "- prog.ext bump filename lineno",
    "- prog.ext remove filename lineno1 lineno2 ...",
    "- prog.exe view filename"
    ]


add :: [String] -> IO ()
add args = do
    Monad.when (length args < 2) $ do
        help
        Exit.exitFailure

    let
        (filename: items) = args

    fileExists <- Dir.doesFileExist filename
    let
        outputToFile = if fileExists then appendFile else writeFile

    outputToFile filename (unlines items)


bump :: [String] -> IO()
bump args = do
    Monad.when (length args /= 2) $ do
        help
        Exit.exitFailure

    let
        filename = args !! 0
        linenumber = (read $ args !! 1) - 1

    items <- readFile filename
    let
        (topItem, tailItems) = popAt linenumber (lines items)

    Exception.bracketOnError (SysIO.openTempFile "./" ".temp")
        (\(tmpfilename, handle) -> do
            SysIO.hClose handle
            Dir.removeFile tmpfilename
        )
        (\(tmpfilename, handle) -> do
            mapM_ (SysIO.hPutStrLn handle) (topItem: tailItems)
            
            SysIO.hClose handle
            Dir.renameFile tmpfilename filename
        )


remove :: [String] -> IO ()
remove args = do
    Monad.when (length args < 2) $ do
        help
        Exit.exitFailure

    let
        filename = args !! 0
        lineNumbers = map read (drop 1 args) :: [Int]

    exitIfFileDoesntExist filename

    Exception.bracketOnError (SysIO.openTempFile "./" ".temp") 
        (\(tmpfilename, handle) -> do
            SysIO.hClose handle
            Dir.removeFile tmpfilename
        )
        (\(tmpfilename, handle) -> do
            items <- readFile filename
            mapM_ (SysIO.hPutStrLn handle) $
                [item | (lineno, item) <- enumerate 1 (lines items), not (lineno `elem` lineNumbers)]

            SysIO.hClose handle
            Dir.renameFile tmpfilename filename
        )


view :: [String] -> IO ()
view args = do
    Monad.when (length args /= 1) $ do
        help
        Exit.exitFailure

    let
        [filename] = args

    exitIfFileDoesntExist filename
    
    items <- readFile filename
    mapM_ putStrLn $ 
        map (\(lineno, item) -> show lineno ++ ". " ++ item) (enumerate 1 (lines items)) 


exitIfFileDoesntExist :: FilePath -> IO ()
exitIfFileDoesntExist filename = do
    fileExists <- Dir.doesFileExist filename
    
    Monad.when (not fileExists) $ do
        putStrLn $ "File " ++ filename ++ " does not exists."
        Exit.exitFailure

