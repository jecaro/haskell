{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import qualified Brick.Main                    as M
import qualified Brick.Widgets.Dialog          as D
import qualified Brick.Widgets.Edit            as E

import           Control.Monad
import           Control.Arrow                            ( (>>>) )

import           Data.List
import           Data.Maybe

import           Lens.Micro

import           Safe

import           System.Console.ANSI
import           System.Environment
import           System.IO
import           System.Random
import           System.Random.Shuffle

import qualified Text.Read                     as TR

import           Game
import           Ui

-- TODO
-- Replace by microlens-platform
-- Esc give up
-- Implement loop
-- h to show the secret word
-- fromJust
-- elemIndex -> lens

-- Get the number of trials from arg list
getNbTrialsFromArgs :: [String] -> Maybe Int
getNbTrialsFromArgs args = do
  ind <- "-n" `elemIndex` args
  el  <- atMay args $ succ ind
  TR.readMaybe el

-- Check the validity of the arg list
checkArgs :: [String] -> Bool
checkArgs ("-n" : x : xs) =
  isJust (TR.readMaybe x :: Maybe Int) && checkArgs xs
checkArgs (x : xs) = x == "-h" && checkArgs xs
checkArgs []       = True

-- Print simple usage
usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ progName ++ ": [-n 10]"
  putStrLn "-n 10\tSet the number of trials (default 10)"

-- Main function
main :: IO ()
main = do
  -- Arguments initialization
  args <- getArgs
  if not (checkArgs args) || "-h" `elem` args
    then usage
    else do
      let nbTrials = fromMaybe 10 $ getNbTrialsFromArgs args
      -- Secret word to guess
      gen <- getStdGen
      -- Init the game and start it
      let game = draw (createGame nbTrials ['0'..'9']) 4 gen
      -- print $ game' ^. getSecret
      -- c <- getChar
      state <- M.defaultMain theApp (createGameState game)
      print $ show $ fromJust $ getAnotherGame state 