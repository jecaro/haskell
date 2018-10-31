{-# LANGUAGE FlexibleContexts #-}

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Reader
import           Data.List
import           Data.Maybe
import           Safe
import           System.Console.ANSI
import           System.Environment
import           System.IO
import           System.Random
import qualified Text.Read as TR

-- TODO
-- handle prompt better with clearScreen

-- State of the game, a stack with the guesses
newtype GameState = GameState{guesses :: [String]}

-- Config of the game
data GameConfig = GameConfig {
  nbTrials :: Int,   -- Number of trials
  values :: String,  -- Possible values for secret
  secret :: String } -- Secret word

-- Status of the game
data Status = Won | Lost | Continue deriving (Eq)

-- Commands handled in the game
data Command = Quit | Guess String deriving (Eq)

-- Take a value at a specific index in a list
-- return this value along the remaining list
pick :: [a] -> Int -> (a, [a])
pick list ind = (list !! ind, start ++ end)
  where
    start = take ind list
    end   = drop (succ ind) list

-- Shuffle a list 
shuffle :: RandomGen g => [a] -> State g [a]
shuffle []   = return []
shuffle list = do
  ind <- state $ randomR (0, length list - 1)
  let (val, list') = pick list ind
  fmap (val :) (shuffle list')

-- Compute the result of the guess
compute :: Eq a => [a] -> [a] -> (Int, Int)
compute secret guess = (goodSpot, good - goodSpot)
 where
  goodSpot = length . filter (uncurry (==)) $ zip secret guess
  good     = length $ filter (`elem` secret) guess

-- Check if a list contains duplicates elements
hasDuplicate :: (Eq a, Ord a) => [a] -> Bool
hasDuplicate w = any (\x -> length x >= 2) $ group $ sort w

-- Quit command
isQuitCmd c = c == "q"

-- Recursive function to get the next command
getNextCmd' :: StateT (Int, String) IO ()
getNextCmd' = do
  -- Get the state
  (n, word) <- get
  -- We're not finished
  unless (n == 0) $ do
    c <- liftIO getChar
    let word' = word ++ [c]
    -- Update state
    put (n - 1, word')
    -- Go recursive
    unless (isQuitCmd word' || c == '\n') getNextCmd'

-- Get input of specific size or quit or ending with \n
-- Int : max number of chars to read before stopping
getNextCmd :: Int -> IO String
getNextCmd n = do
  putStr "> "
  -- Start the recursive function
  (_, cmd) <- execStateT getNextCmd' (n, "")
  -- Output '\n' if needed
  unless (last cmd == '\n') $ putStrLn ""
  -- Get the word
  return cmd

-- Return the validity of the guess
-- - the right length
-- - no repeated letters
-- - correct values
validCmd :: MonadReader (Int, String) m => String -> m Bool
validCmd guess 
    | isQuitCmd guess = return True
    | otherwise = do 
      (n, values) <- ask
      return
        $  (length guess == n)
        && not (hasDuplicate guess)
        && all (`elem` values) guess

-- Loop until it gets a valid command from prompt. It can be:
-- - a quit command
-- - a valid guess
getCmd :: ReaderT (Int, String) IO Command
getCmd = do
  (n, values) <- ask
  liftIO saveCursor
  cmd         <- liftIO $ getNextCmd n
  valid       <- validCmd cmd
  if not valid 
    then do 
      liftIO $ putStrLn "Invalid command"
      getCmd
    else if isQuitCmd cmd
      then return Quit
      else return $ Guess cmd

-- Add guess in the state
addGuess guess gs@(GameState g) = gs { guesses = g ++ [guess] }

-- Print guesses stack
printGuesses :: [String] -> String -> IO ()
printGuesses guesses secret = forM_ (zip [1..] guesses) $ \(i, guess) -> 
    let (g, w) = compute secret guess
    in putStrLn $ show i ++ "-" ++ guess ++ "-" ++ show g ++ " " ++ show w

-- Play loop
play :: ReaderT GameConfig (StateT GameState IO) ()
play = do
  (GameConfig nbTrials values secret) <- ask
  (GameState guesses                ) <- get

  -- Current state
  let current | not (null guesses) && (last guesses == secret) = Won
              | length guesses == nbTrials = Lost
              | otherwise = Continue

  -- Print game status
  liftIO $ printGuesses guesses secret

  -- Branch in the game
  case current of

    -- Game is finished
    Won  -> liftIO $ putStrLn "You won !"
    Lost -> liftIO $ putStrLn "You lose !"
    
    -- Carry on
    Continue -> do

      cmd <- liftIO $ runReaderT getCmd (length secret, values)

      case cmd of
        Quit        -> liftIO $ putStrLn "Ok bye"
        Guess guess -> do
          modify $ addGuess guess
          play

-- Get the number of trials from arg list
getNbTrials :: [String] -> Maybe Int
getNbTrials args = do 
  ind <- "-n" `elemIndex` args
  el <- atMay args $ succ ind
  TR.readMaybe el
                  
-- Check the validity of the arg list
checkArgs :: [String] -> Bool
checkArgs ("-n":x:xs) = isJust (TR.readMaybe x :: Maybe Int) && checkArgs xs
checkArgs (x:xs) = (x == "-d" || x == "-h") && checkArgs xs
checkArgs [] = True

-- Print simple usage
usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ progName ++ ": [-d] [-n 10]" 
  putStrLn "-d\tDebug mode" 
  putStrLn "-n 10\tSet the number of trials (default 10)" 

-- Main function
main :: IO ()
main = do

  -- No buffering mode on stdio
  hSetBuffering stdin NoBuffering

  -- Arguments initialization
  args <- getArgs

  if not (checkArgs args) || "-h" `elem` args
    then usage
    else do
      let debug = "-d" `elem` args
      let nbTrials = fromMaybe 10 $ getNbTrials args

      -- Game configuration
      let nbLetters = 4
      let values    = ['0' .. '9']

      -- Secret word to guess
      gen <- getStdGen
      let secret = take nbLetters $ evalState (shuffle values) gen

      -- Show secret word in debug mode
      when debug $ putStrLn secret

      -- Config of the game
      let config = GameConfig nbTrials values secret

      -- Lets play
      evalStateT (runReaderT play config) $ GameState []
