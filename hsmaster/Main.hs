{-# LANGUAGE FlexibleContexts #-}

import           System.Random
import           System.IO
import           Control.Monad
import           Data.List
import           Control.Monad.State
import           Control.Monad.Reader

-- TODO 
-- clear screen and show stack of guess

-- Take a value at a specific index in a list
-- return this value along the remaining list
pick :: [a] -> Int -> (a, [a])
pick list ind = (list !! ind, start ++ end)
 where
  start = take ind list
  end   = drop (succ ind) list

-- Shuffle a list 
shuffle :: RandomGen g => [a] -> State g [a]
shuffle [] = return []
shuffle list = do
  ind <- state $ randomR (0, length list - 1) 
  let (val, list') = pick list ind
  fmap (val:) (shuffle list') 

-- Compute the result of the guess
compute :: Eq a => [a] -> [a] -> (Int, Int)
compute secret guess = (goodSpot, good - goodSpot)
 where
  goodSpot = length . filter (uncurry (==)) $ zip secret guess
  good     = length $ filter (`elem` secret) guess

hasRepeated :: (Eq a, Ord a) => [a] -> Bool
hasRepeated w = any (\x -> length x >= 2) $ group $ sort w

-- Recursive function to get the next command
getNextCmd' :: StateT (Int, String) IO ()
getNextCmd' = do
  -- Get the state
  (n, word) <- get
  -- Where not finished
  unless (n == 0) $ do 
    c <- liftIO getChar
    let word' = word ++ [c]
    -- Update state
    put (n - 1, word')
    -- Go recursive
    unless (c == 'q' || c == '\n') getNextCmd'

-- Get input of specific size or 'q' or ending with \n
-- Int : max number of chars to read before stopping
getNextCmd :: Int -> IO String
getNextCmd n = do 
  -- Start the recursive function
  s <- execStateT getNextCmd' (n, "")
  -- Get the word
  return $ snd s

-- String ends with \n
endWidthN cmd = last cmd == '\n'

-- Output \n if parameter do not end with it
endOfLineIfNeeded :: String -> IO ()
endOfLineIfNeeded cmd = unless (endWidthN cmd) $ putStrLn ""

-- Return the validity of the guess
-- - the right length
-- - no repeated letters
-- - correct values
validCmd :: MonadReader (Int, String) m => String -> m Bool
validCmd "q" = return True
validCmd guess = do
   (n, values) <- ask
   return $ (length guess == n) && not (hasRepeated guess) && all (`elem` values) guess

-- Loop until it gets a valid command from prompt. It can be
-- 'q' or any valid word
getValidCmd :: ReaderT (Int, String) IO String
getValidCmd = do
  (n, values) <- ask
  cmd <- liftIO $ getNextCmd n
  valid <- validCmd cmd
  if valid
    then do
      liftIO $ endOfLineIfNeeded cmd
      return cmd
    else do
      let cmd' = if endWidthN cmd then init cmd else cmd
      liftIO $ endOfLineIfNeeded cmd
      liftIO $ putStrLn $ "Invalid input: " ++ cmd'
      getValidCmd

-- Number of remaining trials
newtype GameState = GameState{trials :: Int} 

data GameConfig = GameConfig {
  values :: String,  -- Possible values for secret
  secret :: String } -- Secret word

-- Decrement the number of trials
nextTrial gs@(GameState t) = gs { trials = t - 1 }

-- Print output of guess and return True if the game ended
printOutputAndStop :: String -> String -> IO Bool
printOutputAndStop secret "q" = return True
printOutputAndStop secret guess = do
  let won = secret == guess
  if won
    then putStrLn "You won !"
    else do
      let result = compute secret guess
      putStrLn $ "Good       : " ++ show (fst result)
      putStrLn $ "Almost good: " ++ show (snd result)
  return won

play :: ReaderT GameConfig (StateT GameState IO) ()
play = do
  gc@(GameConfig values secret) <- ask
  gs@(GameState trials) <- get 

  if trials == 0 
    then liftIO $ putStrLn "You lose !"
    else do

      cmd <- liftIO $ runReaderT getValidCmd (length secret, values)

      stop <- liftIO $ printOutputAndStop secret cmd

      unless stop $ do
        modify nextTrial 
        play
        
main :: IO ()
main = do

  -- No buffering mode on stdio
  hSetBuffering stdin NoBuffering

  -- Game configuration
  let nbLetters = 4
  let values    = ['0' .. '9']
  let nbTrials  = 3

  -- Secret word to guess
  gen <- getStdGen
  let secret = take nbLetters $ evalState (shuffle values) gen
  putStrLn secret

  -- State of the game
  let state = GameState nbTrials 

  -- Config of the game
  let config = GameConfig values secret

  -- Lets play
  evalStateT (runReaderT play config) state
