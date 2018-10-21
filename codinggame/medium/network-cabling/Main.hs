import System.IO
import Control.Monad

import Data.List
import Data.Ord
 
--import Data.Set (Set)
--import qualified Data.Set as Set

listToTuple :: [Int] -> [(Int, Int)]
listToTuple (x:y:r) = [(x, y)] ++ listToTuple r
listToTuple _ = []

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let n = read input_line :: Int
    
    --posList <- replicateM n $ do
    --    input_line <- getLine
    --    let input = words input_line
    --    let x = read (input!!0) :: Int
    --    let y = read (input!!1) :: Int
    --    return (x, y)
    content <- getContents
    let posList = listToTuple $ map (read :: String -> Int) $ words content
    --hPutStrLn stderr $ show posList
    
    let sortedY = sortBy (comparing snd) posList
    --hPutStrLn stderr $ show sortedY

    let yNet = snd $ sortedY !! ((length sortedY) `div` 2)
    --hPutStrLn stderr $ show yNet    
    
    let sortedX = sortBy (comparing fst) posList
    let xMin = fst $ head sortedX
    let xMax = fst $ last sortedX
    
    let vertDist = sum $ map (\(_, y) -> abs (y - yNet)) posList
    --hPutStrLn stderr $ show vertDist
    
    let answer = xMax - xMin + vertDist
    --let answer = 0
    
    -- Write answer to stdout
    print answer
    return ()
