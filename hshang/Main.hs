module Main where

-- For Exception handling
import           Control.Exception
-- For when
import           Control.Monad.State
import           Lens.Micro.Platform
import           System.Environment
import           System.IO
-- For random numbers
import           System.Random
-- For text -> Int conversion
import qualified Text.Read                     as T

import           Game

-- TODO 
-- check if it is possible to create lens for functions in Game.hs

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
    c <- getChar
    if c `elem` alpha
        then return c 
        else do
            putStrLn ""
            putStrLn "This character is not allowed"
            getAlphaChar    

play :: StateT Game IO ()
play = do

  gs <- get

  -- Print Status
  liftIO $ do
    putStrLn $ "Remaining trials:\t" ++ show (gs ^. getCount)
    putStrLn $ "Guess:\t\t\t"        ++ (gs ^. getHint)
    putStrLn $ "Letters tried:\t\t"  ++ (gs ^. getLetters)

  case gs ^. getStatus  of
    Won  -> liftIO $ putStrLn "You find it !"
    Lost -> liftIO $ putStrLn "No ! You've lost"

    Continue -> do 

      -- Get the char
      c <- liftIO getAlphaChar
      liftIO $ putStrLn ""

      -- Check if we've already got it
      if c `elem` (gs ^. getLetters)
      then liftIO $ putStrLn "You already tried this letter !"
      -- Update state with adding the char
      else put $ addChar gs c

      play 

-- Convert a string to an answer
strToAnswer :: String -> Maybe Answer
strToAnswer "yes" = Just Yes
strToAnswer "no" = Just No
strToAnswer _ = Nothing

-- Answer to the question would play again ?
getAnswer :: IO Answer
getAnswer = do
  cmd <- getLine
  case strToAnswer cmd of
    Nothing -> do 
        putStrLn "I did not understand"
        getAnswer
    Just x -> return x

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
startPlay words count = do

  gen <- newStdGen
  let (val, _) = randomR (0, length words - 1) gen :: (Int, StdGen)
      chosen   = words !! val

  putStrLn "Find the secret word !"

  runStateT play (createGame chosen count)

  putStrLn "Another game ?"

  -- Getting answer
  answer <- getAnswer
  when (answer == Yes) $ startPlay words count

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
          print words
          if null words
            then putStrLn "Empty dictionary"
            else startPlay words count

