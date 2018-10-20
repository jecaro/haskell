module Main where

-- For random numbers
import System.Random
import System.Environment
-- For when
import Control.Monad.State
-- For text -> Int conversion
import Text.Read
-- For Exception handling
import Control.Exception


-- Convert the secret word to the form -x-y---
guess :: String -> String -> String
guess word chars = map (\x -> if x `elem` chars then x else '-') word

-- Play loop
play :: String -> String -> Int -> IO ()
play secret letters count = do

    -- Show the current number of trials
    putStrLn $ "Still " ++ show count ++ " trials"

    -- Get the char
    c <- getChar
    putStrLn ""

    -- Update number of letters
    let letters' = letters ++ [c]

    -- The hint word
    let guess' = guess secret letters'
    putStrLn guess'

    -- Updated remaining turns
    let count' = count - 1

    if count' == 0

        then putStrLn "No ! You've lost"

        else if c `elem` letters
            then do
                putStrLn "Already tried !"
                play secret letters count'
            else if guess' == secret
                 then putStrLn "You find it !"
                 else play secret letters' count'

-- Answer to the question would play again ?
getAnswer :: IO String
getAnswer = do
    cmd <- getLine
    if cmd `elem` ["yes", "no"]
        then return cmd
        else do
            putStrLn "I did not understand"
            getAnswer

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
        chosen = words !! val

    putStrLn "Find the secret word ?"
    
    play chosen [] count
    
    putStrLn "Another game ?"
    
    -- Getting answer
    answer <- getAnswer
    when (answer /= "no") main

main :: IO ()
main = do

    args <- getArgs

    case validateArgs args of 

        Nothing -> usage

        Just (fileName, count) -> do
            
            -- Read dictionary
            dictOrExc <- try (readFile fileName) :: IO (Either SomeException String)
            
            case dictOrExc of 
                
                Left except -> print except
                
                Right dict -> do

                    -- Clean up what we read in the file
                    let words = filter (not . null) $ map sanitize $ lines dict

                    startPlay words count

