import System.IO
import Control.Monad
import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let n = read input_line :: Int
    
    inputs <- replicateM n $ getLine
    let puissances = map read inputs :: [Int]
    
    let sorted = sort(puissances)
    let diff = zipWith (-) (tail sorted) sorted
    
    --hPutStrLn stderr (show(sorted))
    --hPutStrLn stderr (show(diff))
    -- hPutStrLn stderr "Debug messages..."
    
    -- Write answer to stdout
    putStrLn(show(minimum(diff)))
    
    return ()
