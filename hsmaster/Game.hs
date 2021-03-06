{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game
  ( Game
  , Status(..)
  -- Functions
  , createGame
  , draw
  , compute
  , validPartialGuess
  , validGuess                      
  -- Getters
  , getNbTrials
  , getSecret
  , getGuesses
  , getStatus
  -- Setters
  , addGuess
  )
where

import           Control.Monad.State
import           Control.Monad.Random
import           Data.List
import           Lens.Micro.Platform
import           System.Random
import           System.Random.Shuffle

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
addGuess :: String -> Game -> Game
addGuess guess game = game & guesses %~ (guess:)

-- Create a new game
createGame :: Int -> String -> Game
createGame n v = 
  Game { _guesses = []
       , _nbTrials = n
       , _values = v
       , _secret = "" } 

-- Init a new game
draw :: MonadRandom m => Int -> Game -> m Game
draw nbLetters game = do 
  shuffled <- shuffleM $ game ^. values
  let secret' = take nbLetters shuffled
  return $ game & secret .~ secret' & guesses .~ []

-- Compute the status of the game
getStatus' :: Game -> Status
getStatus' game 
  -- no guesses
  | null $ game ^. guesses = Continue
  -- win
  | head (game ^. guesses) == game ^. secret = Won
  -- loose
  | length (game ^. guesses) == game ^. nbTrials = Lost
  -- anything else
  | otherwise = Continue

getStatus :: SimpleGetter Game Status
getStatus = to getStatus'

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

