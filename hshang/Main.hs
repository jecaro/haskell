module Main where

-- For random numbers
import           System.Random
import           System.Environment
-- For when
import           Control.Monad.State
-- For text -> Int conversion
import           Text.Read
-- For Exception handling
import           Control.Exception

-- List of allowable chars
alpha :: String
alpha = ['a'..'z'] ++ ['A'..'Z']

-- Convert the secret word to the form -x-y---
guess :: String -> String -> String
guess word chars = map (\x -> if x `elem` chars then x else '-') word

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

-- Play loop
play :: String -> String -> Int -> IO ()
play secret letters 0     = putStrLn "No ! You've lost"
play secret letters count = do
  -- Show the current number of trials
  putStrLn $ "Still " ++ show count ++ " trials"

  -- Get the char
  c <- getAlphaChar
  putStrLn ""

  if c `elem` letters
    then do
      putStrLn "You already tried this letter !"
      play secret letters count
    else do
      -- Update number of letters
      let letters' = letters ++ [c]

      -- The hint word
      let guess' = guess secret letters'
      putStrLn guess'

      if guess' == secret
        then putStrLn "You find it !"
        else do
            let count' = if c `elem` secret then count else count - 1 
            play secret letters' count'

-- Simple data type to handle an answer
data Answer = Yes | No 
    deriving Eq

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
  count <- readMaybe y
  return (x, count)
validateArgs _ = Nothing

-- Higher level loop
startPlay :: [String] -> Int -> IO ()
startPlay words count = do

  gen <- newStdGen
  let (val, _) = randomR (0, length words - 1) gen :: (Int, StdGen)
      chosen   = words !! val

  putStrLn "Find the secret word ?"

  play chosen [] count

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

