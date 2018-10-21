import System.IO
import Control.Monad

import Data.List.Split
import Data.List
import Data.Tuple

strToBase20 :: [String] -> String -> Int
strToBase20 alpha str = case elemIndex str alpha of 
                        Just v -> v
                        Nothing -> 0

convertBase :: Int -> Int -> [Int]
convertBase base v = reverse $ unfoldr (\x -> if x < 0 
                                          then Nothing 
                                          else if x < base 
                                               then Just (x, -1)
                                               else Just $ swap $ quotRem x base) v

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let input = words input_line
    let l = read (input!!0) :: Int
    let h = read (input!!1) :: Int
    
    numbersStr <- replicateM h getLine
    let alpha = map unlines $ transpose $ map (chunksOf l) numbersStr
    
    let strTo20 = \x -> map (strToBase20 alpha . unlines) (chunksOf h x)
    let n20To10 = \x -> sum $ map (\(i,x) -> x * 20 ^ i) $ zip [0..] $ reverse x
    
    input_line <- getLine
    let s1 = read input_line :: Int
    
    firstStr <- replicateM s1 getLine
    let first = n20To10 $ strTo20 firstStr
    -- hPutStrLn stderr $ show first
    
    input_line <- getLine
    let s2 = read input_line :: Int
    
    secondStr <- replicateM s2 getLine
    let second = n20To10 $ strTo20 secondStr
    -- hPutStrLn stderr $ show second
        
    input_line <- getLine
    let operation = input_line :: String
    
    let result  
            | operation == "+" = first + second
            | operation == "-" = first - second
            | operation == "*" = first * second
            | operation == "/" = first `div` second
    
    -- hPutStrLn stderr $ show result
    
    let base20 = convertBase 20 result
    let base20Str = map (alpha!!) base20
    
    -- hPutStrLn stderr $ show base20
    -- hPutStrLn stderr $ show base20Str
    
    mapM putStr base20Str

    return ()
