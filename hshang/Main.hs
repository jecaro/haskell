module Main where

-- For Exception handling
import           Control.Exception
-- For when
import           Control.Monad.State
import           Control.Monad.Loops
import           Data.Maybe
import           Lens.Micro.Platform
import           System.Environment
import           System.IO
-- For random numbers
import           System.Random
-- For text -> Int conversion
import qualified Text.Read                     as T

import           Game

-- TODO 
-- Improve IO

-- Simple data type to handle an answer
data Answer = Yes | No 
    deriving Eq

-- List of allowable chars
alpha :: String
alpha = ['a'..'z'] ++ ['A'..'Z']

-- Loop until the user send actual char
getAlphaChar :: IO Char
getAlphaChar = do
  putStrLn "What is you character ?"
  untilJust $ do 
    c <- getChar
    if c `elem` alpha
      then return $ Just c
      else do
        putStrLn ""
        putStrLn "This character is not allowed"
        return Nothing

play :: StateT Game IO ()
play = do

  untilM_ (do
    
    -- Print Status
    count <- gets (view getCount)
    liftIO $ putStrLn $ "Remaining trials:\t" ++ show count

    letters <- gets (view getLetters)
    liftIO $ putStrLn $ "Letters tried:\t\t" ++ letters
    
    -- Get the next character
    nextChar <- untilJust $ do

        -- Get a char
        c <- liftIO getAlphaChar
        liftIO $ putStrLn ""
        
        -- Check if we've already got it
        letters <- gets (view getLetters)
        if c `elem` letters
          then do
            liftIO $ putStrLn "You already tried this letter !"
            return Nothing
          else return $ Just c
    
    -- Update state with adding the char
    modify (`addChar` nextChar)
    
    -- Show the guess
    hint <- gets (view getHint)
    liftIO $ putStrLn $ "Guess:\t\t\t" ++ hint
    )
    $ (/= Continue) <$> gets (view getStatus)
  
  status <- gets (view getStatus)
  case status of
    Won  -> liftIO $ putStrLn "You find it !" 
    Lost -> liftIO $ putStrLn "No ! You've lost"
    _    -> liftIO $ putStrLn "Not possible !"

-- Convert a string to an answer
strToAnswer :: String -> Maybe Answer
strToAnswer "yes" = Just Yes
strToAnswer "no" = Just No
strToAnswer _ = Nothing

-- Answer to the question would play again ?
getAnswer :: IO Answer
getAnswer = untilJust $ do 
  cmd <- getLine
  let strToAnswer' = strToAnswer cmd
  when (isNothing strToAnswer') $
    putStrLn "I did not understand"
  return strToAnswer'

-- Clean up a string at the beginning and the end
sanitize :: String -> String
sanitize w = takeWhile (/= ' ') $ dropWhile (== ' ') w

-- Show the usage message
usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " dictFile nbTrials"

-- Test the arguments
validateArgs :: [String] -> Maybe (String, Int)
validateArgs [x, y] = do
  count <- T.readMaybe y
  return (x, count)
validateArgs _ = Nothing

-- Higher level loop
startPlay :: [String] -> Int -> IO ()
startPlay words count = untilM_ (do
  gen <- newStdGen
  let (val, _) = randomR (0, length words - 1) gen :: (Int, StdGen)
      chosen   = words !! val
  putStrLn "Find the secret word !"
  runStateT play (createGame chosen count)
  putStrLn "Another game ?"
  ) $ return (/= Yes) <*> getAnswer

-- Check if a word read in the file is valid
validWord :: String -> Bool
validWord [] = False
validWord w = w == filtered
    where filtered = filter (`elem` alpha) w
              
main :: IO ()
main = do

  hSetBuffering stdin NoBuffering

  args <- getArgs

  case validateArgs args of

    Nothing                -> usage

    Just (fileName, count) -> do

      -- Read dictionary
      dictOrExc <- try (readFile fileName) :: IO (Either SomeException String)

      case dictOrExc of

        Left  except -> print except

        Right dict   -> do

          -- Clean up what we read in the file
          let words = filter validWord $ map sanitize $ lines dict
          if null words
            then putStrLn "Empty dictionary"
            else startPlay words count

