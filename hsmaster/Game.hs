{-# LANGUAGE OverloadedStrings #-}

module Game
  ( Game
  , Status(..)
  -- Functions
  , createGame
  , draw
  , status
  , compute
  , validPartialGuess
  , validGuess
  -- Getters
  , editor
  , nbTrials
  , secret
  , guesses
  -- Setters
  , setEditor
  , applyEditor
  , addGuess
  )
where

import           Control.Monad.State
import           Data.List
import           Lens.Micro
import           Lens.Micro.TH
import           System.Random
import qualified Brick.Widgets.Edit            as E

-- TODO 
-- Use system shuffle

-- Main state of the game
data Game e = Game { editor   :: e
                   , guesses  :: [String]
                   , nbTrials :: Int
                   , values   :: String
                   , secret   :: String  } deriving (Show)

-- Status of the game
data Status = Won | Lost | Continue deriving (Eq)

-- Change the editor
setEditor game e = game { editor = e }

-- Apply a fonction to the editor
applyEditor game fct = game { editor = fct (editor game) }

-- Append guess in front of guesses
addGuess :: Game e -> String -> Game e
addGuess game guess = game { guesses = guess : guesses game }

-- Create a new game
createGame :: Int -> String -> e -> Game e
createGame n v e = 
  Game { editor = e
       , guesses = []
       , nbTrials = n
       , values = v
       , secret = "" } 

-- Take a value at a specific index in a list
-- return this value along the remaining list
pick :: [a] -> Int -> (a, [a])
pick list ind = (list !! ind, start ++ end)
 where
  start = take ind list
  end   = drop (succ ind) list

-- Shuffle a list 
shuffle :: RandomGen g => [a] -> State g [a]
shuffle []   = return []
shuffle list = do
  ind <- state $ randomR (0, length list - 1)
  let (val, list') = pick list ind
  fmap (val :) (shuffle list')

-- Find a new secret for the game
draw :: RandomGen g => Game e -> Int -> g -> Game e
draw game nbLetters gen = game { secret = secret'}
  where secret' = take nbLetters $ evalState (shuffle $ values game) gen 

-- Compute the status of the game
status :: Game e -> Status
status game 
  -- no guesses
  | null $ guesses game = Continue
  -- win
  | head (guesses game) == secret game = Won
  -- loose
  | length (guesses game) == nbTrials game = Lost
  -- anything else
  | otherwise = Continue

-- Compute the result of the guess
compute :: Game e -> String -> (Int, Int)
compute game guess = (goodSpot, good - goodSpot)
 where
  s = secret game
  goodSpot = length . filter (uncurry (==)) $ zip s guess
  good     = length $ filter (`elem` s) guess

-- Return the validity of the guess
-- - the right length
-- - no repeated letters
-- - correct values
validPartialGuess :: Game a -> String -> Bool
validPartialGuess (Game _ guesses _ values secret) guess = 
  (length guess <= n) && not (hasDuplicate guess) && all (`elem` values) guess
    where 
      n = length secret
      hasDuplicate w = any (\x -> length x >= 2) $ group $ sort w

-- Return the validity of the guess, partially good with the good length
validGuess :: Game a -> String -> Bool
validGuess g guess = validPartialGuess g guess && (length guess == length (secret g))

