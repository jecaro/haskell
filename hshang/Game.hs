{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Game 
  ( Status(..)
  , GameState
  , initGame
  , getHint
  , getCount
  , getLetters
  , getStatus
  , addChar
  )
where

import           Lens.Micro.Platform
 
data Status = Won | Lost | Continue

data GameState = GameState { _secret  :: String
                           , _letters :: String
                           , _count   :: Int}
makeLenses ''GameState

initGame :: String -> Int -> GameState
initGame word = GameState word [] 

-- Convert the secret word to the form -x-y---
getHint :: GameState -> String
getHint GameState{_secret = secret, _letters = letters} = 
  map (\x -> if x `elem` letters then x else '-') secret

getCount :: SimpleGetter GameState Int
getCount = count

getLetters :: SimpleGetter GameState String
getLetters = letters

addChar :: GameState -> Char -> GameState
addChar gs@GameState { _secret = s, _letters = l } c
  | c `elem` l = gs
  | c `elem` s = gs & letters %~ ([c] ++)
  | otherwise  = gs & letters %~ ([c] ++) & count %~ pred 
  
getStatus :: GameState -> Status
getStatus GameState { _count = 0 } = Lost
getStatus gs@GameState { _secret = secret }
  | getHint gs == secret = Won
  | otherwise = Continue 
