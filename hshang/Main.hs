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

-- Simple data type to handle an answer
data Answer = Yes | No 
    deriving Eq

-- List of allowable chars
alpha :: String
alpha = ['a'..'z'] ++ ['A'..'Z']

-- Loop until the user send a valid char
getValidChar :: String -> IO Char
getValidChar letters = do

  putStr "Enter a character: "

  untilJust $ do 
    -- Get the char
    c <- toLower <$> getChar
    -- Return go back up
    when (c == '\n') $
      cursorUpLine 1
    -- Erase entire line
    setCursorColumn 0
    clearFromCursorToLineEnd
    -- Check if the char is valid
    let errorMsg = case (c `elem` alpha, c `elem` letters) of
                     (False, _) -> Just $ "The character " ++ show c ++ " is not allowed, try again: "
                     (_, True)  -> Just $ "You already tried " ++ show c ++ ", try again: "
                     _          -> Nothing
    -- Return result
    case errorMsg of
      Just str -> do
        putStr str
        return Nothing 
      Nothing -> do
        putStrLn $ "Your char:\t\t" ++ [c]
        return $ Just c

play :: StateT Game IO ()
play = do

  untilM_ (do
    
    -- Print Status
    count <- use getCount
    liftIO $ putStrLn $ "Remaining trials:\t" ++ show count

    letters <- use getLetters
    liftIO $ putStrLn $ "Letters tried:\t\t" ++ letters
    
    -- Get the next character
    c <- liftIO $ getValidChar letters
   
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
strToAnswer "no"  = Just No
strToAnswer _     = Nothing

-- Answer to the question would play again ?
getAnswer :: IO Answer
getAnswer = do
  
  putStr "Another game ? "

  untilJust $ do 
  
    strToAnswer' <- strToAnswer <$> getLine

    when (isNothing strToAnswer') $ do
      cursorUpLine 1
      clearFromCursorToLineEnd 
      putStr "I did not understand, try again "

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
  -- Pick up random word
  gen <- newStdGen
  -- Need to refactor with safer version
  let (val, _) = randomR (0, length words - 1) gen
      chosen   = words !! val

  putStrLn "Find the secret word !"
  
  runStateT play (createGame chosen count)
  ) $ (/= Yes) <$> getAnswer

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

