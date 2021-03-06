{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import qualified Brick.Main                    as M
import qualified Brick.Widgets.Dialog          as D
import qualified Brick.Widgets.Edit            as E

import           Control.Arrow                            ( (>>>) )
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Random

import           Data.List
import           Data.Maybe

import           Lens.Micro.Platform

import           Safe

import           System.Console.ANSI
import           System.Environment
import           System.IO
import           System.Random
import           System.Random.Shuffle

import qualified Text.Read                     as TR

import           Game
import           Ui

-- Get the number of trials from arg list
nbTrialsFromArgs :: [String] -> Maybe Int
nbTrialsFromArgs args = do
  ind <- "-n" `elemIndex` args
  el  <- atMay args $ succ ind
  nbTrialsFromString el
 
nbTrialsFromString :: String -> Maybe Int
nbTrialsFromString x = do 
  nbTrials <- TR.readMaybe x
  guard $ nbTrials >= 1
  return nbTrials

-- Check the validity of the arg list 
checkArgs :: [String] -> Bool
checkArgs ("-n" : x : xs) =
  isJust (nbTrialsFromString x) && checkArgs xs
checkArgs (x : xs) = x == "-h" && checkArgs xs
checkArgs []       = True

-- Print simple usage
usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ progName ++ ": [-n 10]"
  putStrLn "-n 10\tSet the number of trials (default 10)"

-- Modify the state with a monad action
modifyM :: MonadState s m => (s -> m s) -> m ()
modifyM f = get >>= f >>= put

-- Main play function
play :: RandomGen g => ReaderT Int (StateT Game (RandT g IO)) ()
play = void $ untilJust $ do

  -- Modify the state with drawing a new one
  modifyM . draw =<< ask
  
  -- Get the game and play
  game <- get
  state <- liftIO $ M.defaultMain theApp (createGameState game)
  
  -- Check if the user wants to play again
  return $ case getAnotherGame state of
    Just False -> Just False
    _          -> Nothing

-- Main function
main :: IO ()
main = do
  -- Arguments initialization
  args <- getArgs
  if not (checkArgs args) || "-h" `elem` args
    then usage
    else do
      let nbTrials = fromMaybe 10 $ nbTrialsFromArgs args
      -- Init the game 
      let game = createGame nbTrials ['0' .. '9']
      -- Start the game
      evalRandT (evalStateT (runReaderT play 4) game) =<< getStdGen  
