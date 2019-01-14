module Main where

-- For Exception handling
import           Control.Exception
-- For when
import           Control.Monad.State
import           Control.Monad.Loops
import           Data.Maybe
import           Data.Char
import           Lens.Micro.Platform
import           System.Console.ANSI
import           System.Environment
import           System.IO
-- For random numbers
import           System.Random
-- For text -> Int conversion
import qualified Text.Read                     as T

import           Game

-- TODO 
-- Improve IO
-- Handle \n

-- Simple data type to handle an answer
data Answer = Yes | No 
    deriving Eq

-- List of allowable chars
alpha :: String
alpha = ['a'..'z'] ++ ['A'..'Z']

-- Loop until the user send a valid char
getValidChar :: String -> IO Char
getValidChar letters = do

  saveCursor
  putStr "Enter a character: "

  untilJust $ do 
    -- Get the char
    c <- toLower <$> getChar
    -- Restore cursor and clear up line
    restoreCursor
    clearFromCursorToLineEnd
    -- Check if the char is valid
    let errorMsg = case (c `elem` alpha, c `elem` letters) of
                     (False, _) -> Just $ "The character " ++ [c] ++ " is not allowed, try again: "
                     (_, True)  -> Just $ "You already tried " ++ [c] ++ ", try again: "
                     _          -> Nothing
    -- Return result
    case errorMsg of
      Nothing -> return $ Just c
      Just str -> do
        putStr str
        return Nothing 

play :: StateT Game IO ()
play = do

  untilM_ (do
    
    -- Print Status
    count <- use getCount
    liftIO $ putStrLn $ "Remaining trials:\t" ++ show count

    letters <- use getLetters
    liftIO $ putStrLn $ "Letters tried:\t\t" ++ letters
    
    -- Get the next character
    letters <- use getLetters
    c <- liftIO $ getValidChar letters
   
    -- Pass a line
    liftIO $ putStrLn ""
            
    -- Update state by adding the char
    modify (`addChar` c)
    
    -- Show the guess
    hint <- use getHint
    liftIO $ putStrLn $ "Guess:\t\t\t" ++ hint
    )
    $ (/= Continue) <$> use getStatus
  
  status <- use getStatus
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
  -- Need to refactor with safer version
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
          let words = filter validWord $ map (map toLower . sanitize) $ lines dict
          if null words
            then putStrLn "Empty dictionary"
            else startPlay words count

