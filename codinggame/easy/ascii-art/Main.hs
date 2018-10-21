import System.IO
import Control.Monad
import qualified Data.Map as Map
import Data.List.Split
import Data.Maybe
import Data.List
import Data.Char

outputLetter :: [String] -> Char -> String
outputLetter theme c = theme !! index
    where index = case elemIndex (toLower c) ['a'..'z'] of Just c -> c 
                                                           Nothing -> 26

outputLine :: [[String]] -> String -> Int -> String
outputLine theme word i = concat (map (outputLetter (theme !! i)) word)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let l = read input_line :: Int
    input_line <- getLine
    let h = read input_line :: Int
    word <- getLine
    
    theme <- replicateM h $ do
        row <- getLine
        let rows = chunksOf l row
        return rows

    --hPutStrLn stderr $ show theme
    hPutStrLn stderr word
    
    let lines = [0 .. h - 1]
    let answer = map (outputLine theme word) lines
    
    -- hPutStrLn stderr $ show answer
    mapM putStrLn answer

    
    return ()
