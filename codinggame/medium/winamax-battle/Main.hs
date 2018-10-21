import System.IO
import Control.Monad

import Data.Ord

import qualified Data.Map.Strict as Map

type Cards = [Int]

data Game = PAT | 
    Game { cards1 :: Cards
         , cards2 :: Cards
         , tas :: [(Int, Int)]
         , count :: Int} deriving (Show)

type MapCard = Map.Map String Int

mapCard :: MapCard
mapCard = Map.fromList [
    ("2D",  2),  ("2H",  2),  ("2C",  2),  ("2S",  2),
    ("3D",  3),  ("3H",  3),  ("3C",  3),  ("3S",  3),
    ("4D",  4),  ("4H",  4),  ("4C",  4),  ("4S",  4),
    ("5D",  5),  ("5H",  5),  ("5C",  5),  ("5S",  5),
    ("6D",  6),  ("6H",  6),  ("6C",  6),  ("6S",  6),
    ("7D",  7),  ("7H",  7),  ("7C",  7),  ("7S",  7),
    ("8D",  8),  ("8H",  8),  ("8C",  8),  ("8S",  8),
    ("9D",  9),  ("9H",  9),  ("9C",  9),  ("9S",  9),
    ("10D", 10), ("10H", 10), ("10C", 10), ("10S", 10),
    ("JD",  11), ("JH",  11), ("JC",  11), ("JS",  11),
    ("QD",  12), ("QH",  12), ("QC",  12), ("QS",  12),
    ("KD",  13), ("KH",  13), ("KC",  13), ("KS",  13),
    ("AD",  14), ("AH",  14), ("AC",  14), ("AS",  14)]

score :: String -> Int
score card = case Map.lookup card mapCard of
        Just v -> v
        Nothing -> 0
        

--score :: String -> Int
--score ('2':_) = 2
--score ('3':_) = 3
--score ('4':_) = 4
--score ('5':_) = 5
--score ('6':_) = 6
--score ('7':_) = 7
--score ('8':_) = 8
--score ('9':_) = 9
--score ('1':'0':_) = 10
--score ('J':_) = 11
--score ('Q':_) = 12
--score ('K':_) = 13
--score ('A':_) = 14

--cmp :: String -> String -> Ordering
--cmp s1 s2 = compare (score s1) (score s2)

cmp :: Int -> Int -> Ordering
cmp s1 s2 = compare s1 s2

play :: Game -> Game
play (Game cs1@(c1:r1) cs2@(c2:r2) t c) 
        | cmp c1 c2 == LT = Game r1 (r2 ++ (ramasse t')) [] (c + 1)
        | cmp c1 c2 == GT = Game (r1 ++ (ramasse t')) r2 [] (c + 1)
        | length cs1 < 4 || length cs2 < 4 = PAT
        | otherwise = play (Game r1' r2' t'' c)
            where t' = t ++ [(c1, c2)]
                  ramasse x = let unzipped = unzip x
                              in fst unzipped ++ snd unzipped
                  (d1, r1') = splitAt 4 cs1
                  (d2, r2') = splitAt 4 cs2
                  t'' = t ++ zip d1 d2

play' :: Game -> Game
play' game@(Game c1 [] t c) 
        | null t = game
        | otherwise = PAT
play' game@(Game [] c2 t c) 
        | null t = game
        | otherwise = PAT
play' PAT = PAT
play' game = play' $ play game

finished :: Game -> Bool
finished PAT = True
finished (Game c1 [] t c) = True
finished (Game [] c2 t c) = True
finished _ = False

debugPlay :: Game -> IO ()
debugPlay game = do
    hPutStrLn stderr $ show game
    when (not $ finished game) $ do
        debugPlay $ play game

main :: IO ()
main = do
    --hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    input_line <- getLine
    let n = read input_line :: Int -- the number of cards for player 1
    
    c1 <- replicateM n getLine
        
    input_line <- getLine
    let m = read input_line :: Int -- the number of cards for player 2
    
    c2 <- replicateM n getLine
    
    let game = play' $ Game (map score c1) (map score c2) [] 0
    --hPutStrLn stderr $ show game

    --debugPlay $ Game (map score c1) (map score c2) [] 0

    -- Write answer to stdout
    let answer = case game of
            PAT -> "PAT"
            otherwise -> let winner = if null (cards2 game) then "1" else "2" 
                         in winner ++ " " ++ show (count game)

    putStrLn answer

    return ()
