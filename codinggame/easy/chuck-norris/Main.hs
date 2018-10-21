import System.IO
import Control.Monad
import Data.Bits
import Data.Char
import Data.List

charToBits :: Char -> [Bool]
charToBits c = tail $ map (testBit (ord c)) [7,6..0]

reduce :: Bool -> [(Bool, Int)] -> [(Bool, Int)]
reduce a [] = [(a, 1)]
reduce a xs@((x, y) : rest) 
    | a == x = [(x, y + 1)] ++ rest
    | otherwise = [(a, 1)] ++ xs

chuck :: (Bool, Int) -> String
chuck (x, y)
    | x == True = "0 " ++ replicate y '0'
    | otherwise = "00 " ++ replicate y '0'

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    message <- getLine
    
    let bits = concat $ map charToBits message
    
    let consecutive = foldr reduce [] bits
    
    --hPutStrLn stderr (show bits)
    --hPutStrLn stderr (show consecutive)
    
    -- Write answer to stdout
    let answer = intercalate " " $ map chuck consecutive
    putStrLn answer
    
    return ()
