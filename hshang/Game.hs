{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Game 
  ( Status(..)
  , Game
  -- Game creation
  , createGame
  -- Getters
  , getHint
  , getCount
  , getLetters
  , getStatus
  -- Function
  , addChar
  )
where

import           Data.List
import           Lens.Micro.Platform
 
data Status = Won | Lost | Continue deriving Eq

data Game = Game { _secret  :: String
                 , _letters :: String
                 , _count   :: Int} 
makeLenses ''Game

createGame :: String -> Int -> Game
createGame word = Game word [] 

-- Convert the secret word to the form -x-y---
getHint' :: Game -> String
getHint' Game { _secret = secret, _letters = letters } = 
  map (\x -> if x `elem` letters then x else '-') secret

getHint :: SimpleGetter Game String
getHint = to getHint'

getCount :: SimpleGetter Game Int
getCount = count

getLetters :: SimpleGetter Game String
getLetters = letters

addChar :: Game -> Char -> Game
addChar gs@Game { _secret = s, _letters = l } c
  | c `elem` l = gs
  | c `elem` s = gs & letters %~ insert c
  | otherwise  = gs & letters %~ insert c & count %~ pred 
  
getStatus' :: Game -> Status
getStatus' Game { _count = 0 } = Lost
getStatus' gs@Game { _secret = secret }
  | gs ^. getHint == secret = Won
  | otherwise = Continue 

getStatus :: SimpleGetter Game Status
getStatus = to getStatus'
