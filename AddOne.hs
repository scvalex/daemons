import Data.Default ( def )
import System.Environment ( getArgs )
import System.Daemon

addOne :: Int -> IO Int
addOne n = return (n + 1)

main :: IO ()
main = do
    startDaemon "addOne" def addOne
    [n] <- getArgs
    res <- runClient "localhost" 5000 ((read n) :: Int)
    print (res :: Maybe Int)
