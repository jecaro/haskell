import           System.Random
import           System.IO
import           Control.Monad
import           Data.List
import           Control.Monad.State

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
valid guess n values =
  (length guess == n) && not (hasRepeated guess) && all (`elem` values) guess

-- Loop until it gets a valid command from prompt. It can be
-- 'q' or any valid word
-- TODO use StateT
getValidCmd :: Int -> String -> IO String
getValidCmd n values = do
  cmd <- getNextCmd n
  if cmd == "q" || valid cmd n values
    then do
      endOfLineIfNeeded cmd
      return cmd
    else do
      let cmd' = if endWidthN cmd then init cmd else cmd
      endOfLineIfNeeded cmd
      putStrLn $ "Invalid input: " ++ cmd'
      getValidCmd n values

-- TODO pass values and secret in Reader Monad
data GameConfig = GameConfig {
    trials :: Int,     -- Number of remaining trials
    values :: String,  -- Possible values for secret
    secret :: String } -- Secret word

-- Decrement the number of trials
nextTrial gc@(GameConfig t _ _) = gc { trials = t - 1 }

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

play :: StateT GameConfig IO ()
play = do
  gc@(GameConfig trials values secret) <- get 

  if trials == 0 
    then liftIO $ putStrLn "You lose !"
    else do

      let nbLetters = length secret

      cmd <- liftIO $ getValidCmd nbLetters values

      stop <- liftIO $ printOutputAndStop secret cmd

      unless stop $ do
        modify nextTrial 
        play

main :: IO ()
main = do
  -- Game configuration
  let nbLetters = 4
  let values    = ['0' .. '9']
  let nbTrials  = 3

  -- Secret word to guess
  gen <- getStdGen
  let secret = take nbLetters $ evalState (shuffle values) gen
  putStrLn secret

  -- Configuration of the game
  let config = GameConfig nbTrials values secret

  -- Lets play
  evalStateT play config
