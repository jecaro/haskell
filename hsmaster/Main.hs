import System.Random
import System.IO
import Control.Monad
import Data.List

-- TODO 
-- clear screen and show stack of guess
-- max number of trials

-- Take a value at a specific index in a list
-- return this value along the remaining list
pick :: [a] -> Int -> (a, [a])
pick list ind = (list !! ind, start ++ end)
    where start = take ind list
          end = drop (succ ind) list

-- Shuffle a list
shuffle :: RandomGen g => [a] -> g -> [a]
shuffle [] gen = []
shuffle list gen = let (ind, gen') = randomR (0, length list - 1) gen 
                       (val, list') = pick list ind
                   in val : shuffle list' gen'

-- Compute the result of the guess
compute :: Eq a => [a] -> [a] -> (Int, Int)
compute secret guess = (goodSpot, good - goodSpot) 
    where goodSpot = length . filter (uncurry (==)) $ zip secret guess
          good = length $ filter (`elem` secret) guess

hasRepeated :: (Eq a, Ord a) => [a] -> Bool
hasRepeated w = any (\x -> length x >= 2) $ group $ sort w 

-- Get input of specific size or 'q' or ending with \n
getNextCmd :: Int -> String -> IO String
getNextCmd 0 word = return word
getNextCmd maxLength word = do
    c <- getChar
    let word' = word ++ [c]
    if c == 'q' || c == '\n' 
        then return word' 
        else getNextCmd (maxLength - 1) word'

-- String ends with \n
endWidthN cmd = last cmd == '\n'

-- Output \n if parameter do not end with it
endOfLineIfNeeded cmd = unless (endWidthN cmd) $ putStrLn ""

-- Return the validity of the guess
-- - the right length
-- - no repeated letters
-- - correct values
valid guess n values = (length guess == n) && not (hasRepeated guess) && all (`elem` values) guess

-- Loop until it gets a valid command from prompt. It can be
-- 'q' or any valid word
getValidCmd :: Int -> String -> IO String
getValidCmd n values = do
    cmd <- getNextCmd n ""
    if cmd == "q" || valid cmd n values
        then return cmd
        else do
            let cmd' = if endWidthN cmd then init cmd else cmd
            endOfLineIfNeeded cmd
            putStrLn $ "Invalid input: " ++ cmd'
            getValidCmd n values

-- Play loop
play :: Int -> String -> String -> IO ()
play n values secret = do
    -- Read the first char
    cmd <- getValidCmd n values
    endOfLineIfNeeded cmd
    unless (cmd == "q") $ do
        let result = compute secret cmd
        if fst result == n 
            then putStrLn "You won !"
            else do
                putStrLn $ "Good       : " ++ show (fst result)
                putStrLn $ "Almost good: " ++ show (snd result)
                play n values secret

main :: IO ()
main = do
    -- Number of letters to guess
    let n = 4
    
    -- Current IO generator
    gen <- getStdGen

    -- The values to choose among
    let values = ['0'..'9']

    -- Secret word to guess
    let secret = take n $ shuffle values gen
    putStrLn secret

    -- Lets play
    play n values secret