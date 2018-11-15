{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
  , getNbTrials
  , getSecret
  , getGuesses
  -- Setters
  , addGuess
  )
where

import           Control.Monad.State
import           Data.List
import           Lens.Micro
import           Lens.Micro.TH
import           Lens.Micro.Type
import           System.Random

-- TODO 
-- Use system shuffle

-- Main state of the game
data Game = Game { _guesses  :: [String]
                 , _nbTrials :: Int
                 , _values   :: String
                 , _secret   :: String  } deriving (Show)
makeLenses ''Game
                 
-- Status of the game
data Status = Won | Lost | Continue deriving (Eq)

-- Read only lens
getNbTrials :: SimpleGetter Game Int
getNbTrials = nbTrials 

-- Read only lens
getSecret :: SimpleGetter Game String
getSecret = secret 

-- Read only lens
getGuesses :: SimpleGetter Game [String]
getGuesses = guesses 

-- Append guess in front of guesses
addGuess :: Game -> String -> Game
addGuess game guess = game & guesses %~ (guess:)

-- Create a new game
createGame :: Int -> String -> Game
createGame n v = 
  Game { _guesses = []
       , _nbTrials = n
       , _values = v
       , _secret = "" } 

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
draw :: RandomGen g => Game -> Int -> g -> Game
draw game nbLetters gen = game & secret .~ secret'
  where secret' = take nbLetters $ evalState (shuffle $ game ^. values) gen 

-- Compute the status of the game
status :: Game -> Status
status game 
  -- no guesses
  | null $ game ^. guesses = Continue
  -- win
  | head (game ^. guesses) == game ^. secret = Won
  -- loose
  | length (game ^. guesses) == game ^. nbTrials = Lost
  -- anything else
  | otherwise = Continue

-- Compute the result of the guess
compute :: Game -> String -> (Int, Int)
compute game guess = (goodSpot, good - goodSpot)
 where
  s = _secret game
  goodSpot = length . filter (uncurry (==)) $ zip s guess
  good     = length $ filter (`elem` s) guess

-- Return the validity of the guess
-- - the right length
-- - no repeated letters
-- - correct values
validPartialGuess :: Game -> String -> Bool
validPartialGuess (Game guesses _ values secret) guess = 
  (length guess <= n) && not (hasDuplicate guess) && all (`elem` values) guess
    where 
      n = length secret
      hasDuplicate w = any (\x -> length x >= 2) $ group $ sort w

-- Return the validity of the guess, partially good with the good length
validGuess :: Game -> String -> Bool
validGuess g guess = validPartialGuess g guess && (length guess == length (g ^. secret))

