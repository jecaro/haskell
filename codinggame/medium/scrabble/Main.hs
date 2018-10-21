import System.IO
import Control.Monad

import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List

type MapChar = Map.Map Char Int
type SetWords = Set.Set String

data Score = Invalid | Score { setWords :: SetWords
                             , bestWord :: String
                             , bestScore :: Int} deriving (Show)

mapChar :: MapChar
mapChar = 
    let one = zip ['e', 'a', 'i', 'o', 'n', 'r', 't', 'l', 's', 'u'] $ repeat 1
        two = zip ['d', 'g'] $ repeat 2
        three = zip ['b', 'c', 'm', 'p'] $ repeat 3
        four = zip ['f', 'h', 'v', 'w', 'y'] $ repeat 4
        five = zip ['k'] $ repeat 5
        height = zip ['j', 'x'] $ repeat 8
        ten = zip ['q', 'z'] $ repeat 10
    in Map.fromList $ one ++ two ++ three ++ four ++ five ++ height ++ ten

scoreC :: Char -> Int
scoreC c = case Map.lookup c mapChar of
    Just v -> v
    Nothing -> 0

searchLetter :: (MapChar, Int) -> Char -> (MapChar, Int)
searchLetter (m, s) c = case Map.lookup c m of
        Nothing -> (Map.empty, 0)
        Just v -> let newMap = if v == 1 
                               then Map.delete c m
                               else Map.adjust ((+) (-1)) c m
                  in (newMap, s + scoreC c)

scoreW :: String -> MapChar -> Int
scoreW str letters = snd $ foldl' searchLetter (letters, 0) str

compute :: (Score, MapChar) -> String -> (Score, MapChar)
compute (Invalid, letters) str = (Score (Set.singleton str) str (scoreW str letters), letters)
compute (sc@(Score set bestW bestS), letters) str = if Set.member str set 
    -- Le mot est dans la liste rien a faire
    then (sc, letters)
    -- Il n'y est pas on calcule son score et on met a jour le set
    else let strS = scoreW str letters
             newSet = Set.insert str set
         -- Si le score est meilleur
         in if strS > bestS 
            then (Score newSet str strS, letters)
            else (Score newSet bestW bestS, letters)
  
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let n = read input_line :: Int
    
    dict <- replicateM n $ do
        w <- getLine
        return w
    
    letters <- getLine
    
    let letterMap = foldl (\x c -> Map.insertWith (+) c 1 x) Map.empty letters
    --hPutStrLn stderr $ show letterMap
    
    let scores = fst $ foldl compute (Invalid, letterMap) dict
    --hPutStrLn stderr $ show scores
    
    -- Write answer to stdout
    putStrLn $ bestWord scores
    
    return ()
