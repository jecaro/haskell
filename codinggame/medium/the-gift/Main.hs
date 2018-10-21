import System.IO

import Control.Monad

import Data.List

resolve :: [Int] -> Int -> [Int]
resolve [] c = []
resolve bs@(b:q) c = let (m, r) = quotRem c (length bs)
                         ceil = if r /= 0 then m + 1 else m                         
    in if b < ceil
       then b:(resolve q (c - b))
       else let len = length bs
                base = take len $ repeat m 
                complete = take (len - r) (repeat 0) ++ take r (repeat 1)
            in zipWith (+) base complete

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let n = read input_line :: Int
    input_line <- getLine
    let c = read input_line :: Int
    
    budgets <- replicateM n $ do
        input_line <- getLine
        let b = read input_line :: Int
        return b
    
    let sorted = sort budgets
    let result = resolve sorted c
    
    --hPutStrLn stderr $ show sorted
    --hPutStrLn stderr $ show c
    --hPutStrLn stderr $ show result
    
    -- Write answer to stdout
    if sum budgets < c 
    then putStrLn "IMPOSSIBLE"
    else putStr $ unlines $ map show result
    
    return ()
