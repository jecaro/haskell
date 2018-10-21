import System.IO
import Control.Monad

import Data.List

foldFct :: Int -> [(Int, Int)] -> [(Int, Int)]
foldFct x curr@((n, v):r) 
        | x == v = [(n + 1, v)] ++ r
        | otherwise = [(1, x)] ++ curr
foldFct x [] = [(1, x)]

solve :: [Int] -> Int -> [Int]
solve current 1 = current
solve current level = solve newList (level - 1)
    where count = foldr foldFct [] current
          newList = foldl (\curr x -> curr ++ [fst x, snd x]) [] count

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let r = read input_line :: Int
    
    input_line <- getLine
    let l = read input_line :: Int

    let answer = concat $ intersperse " " $ map show $ solve [r] l
    
    putStrLn $ answer
        
    return ()
