{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Game 
  ( Status(..)
  , Game
  , initGame
  , getHint
  , getCount
  , getLetters
  , getStatus
  , addChar
  )
where

import           Data.List
import           Lens.Micro.Platform
 
data Status = Won | Lost | Continue

data Game = Game { _secret  :: String
                 , _letters :: String
                 , _count   :: Int}
makeLenses ''Game

initGame :: String -> Int -> Game
initGame word = Game word [] 

-- Convert the secret word to the form -x-y---
getHint :: Game -> String
getHint Game { _secret = secret, _letters = letters } = 
  map (\x -> if x `elem` letters then x else '-') secret

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
  | getHint gs == secret = Won
  | otherwise = Continue 

getStatus :: SimpleGetter Game Status
getStatus = to getStatus'
