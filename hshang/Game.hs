{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Game 
  ( Status(..)
  , GameState
  , initGame
  , getHint
  , getCount
  , getLetters
  , getSecret
  , addChar
  , getStatus
  )
where

import           Control.Monad.State
import           Lens.Micro.Platform
 
data Status = Won | Lost | Continue

data GameState = GameState { _secret  :: String
                           , _letters :: String
                           , _count   :: Int}
makeLenses ''GameState

initGame :: String -> Int -> GameState
initGame word count = GameState word [] count

-- Convert the secret word to the form -x-y---
getHint :: MonadState GameState m => m String
getHint = do
  word    <- getSecret
  letters <- getLetters
  return $ map (\x -> if x `elem` letters then x else '-') word

getCount :: MonadState GameState m => m Int
getCount = do
  GameState { _count = count } <- get
  return count

getLetters :: MonadState GameState m => m String
getLetters = do
  GameState { _letters = letters } <- get
  return letters

getSecret :: MonadState GameState m => m String
getSecret = do
  GameState { _secret = secret } <- get
  return secret

addChar :: MonadState GameState m => Char -> m ()
addChar c = do
  gs <- get
  let update = if c `elem` (gs ^. secret) then id else flip (-) 1
  put $ gs & count %~ update & letters %~ ([c] ++)

getStatus :: MonadState GameState m => m Status
getStatus = do 
  count <- getCount
  if count == 0 
  then return Lost 
  else do 
    hint <- getHint
    secret <- getSecret
    if secret == hint 
    then return Won
    else return Continue
