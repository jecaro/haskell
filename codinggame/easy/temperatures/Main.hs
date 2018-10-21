import System.IO
import Control.Monad
import Data.List
import Data.Ord

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let n = read input_line :: Int -- the number of temperatures to analyse
    temps <- getLine
    
    -- the n temperatures expressed as integers ranging from -273 to 5526
    let temperatures = map read $ words temps :: [Int]

    print (solve(temperatures))
                
    return ()
    
solve :: [Int] -> Int
solve[] = 0
solve temperatures =
    let absolutes = map abs temperatures
        minAll = minimumBy (comparing fst) (zip absolutes [0..])
        minIndex = snd(minAll)
        minValue = temperatures!!minIndex
        
    in if minValue < 0 && elem (- minValue) (temperatures)
        then abs(minValue)
        else minValue
