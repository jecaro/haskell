import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let input = words input_line
    let w = read (input!!0) :: Int -- number of columns.
    let h = read (input!!1) :: Int -- number of rows.
    
    matrix <- replicateM h $ do
        line <- getLine
        -- represents a line in the grid and contains W integers. 
        -- Each integer represents one room of a given type.
        let lineList = map (read :: String->Int) (words line)
        return lineList
        
    --hPutStrLn stderr (show matrix)
    
    input_line <- getLine

    -- the coordinate along the X axis of the exit (not useful for 
    -- this first mission, but must be read).
    let ex = read input_line :: Int 
    loop matrix

loop :: [[Int]] -> IO ()
loop matrix = do
    input_line <- getLine
    let input = words input_line
    let xi = read (input!!0) :: Int
    let yi = read (input!!1) :: Int
    let pos = input!!2
    
    --hPutStrLn stderr $ (show xi) ++ " " ++ (show yi)
    
    let current = matrix !! yi !! xi
    
    let out
            | current == 1 = (0, 1)
            | current == 2 = if pos == "LEFT" then (1, 0) else (-1, 0)
            | current == 3 = (0, 1)
            | current == 4 = if pos == "TOP" then (- 1, 0) else (0, 1)
            | current == 5 = if pos == "TOP" then (1, 0) else (0, 1)
            | current == 6 = if pos == "LEFT" then (1, 0) else (-1, 0)
            | current == 7 = (0, 1)
            | current == 8 = (0, 1)
            | current == 9 = (0, 1)
            | current == 10 = (-1, 0)
            | current == 11 = (1, 0)
            | current == 12 = (0, 1)
            | current == 13 = (0, 1)
            | otherwise = (0, 0)
    
    -- hPutStrLn stderr "Debug messages..."
    --hPutStrLn stderr $ show current
    --hPutStrLn stderr $ show out
    
    let newX = xi + (fst out)
    let newY = yi + (snd out)
    -- One line containing the X Y coordinates of the room in 
    -- which you believe Indy will be on the next turn.
    
    putStrLn $ (show newX) ++ " " ++ (show newY)
    
    loop matrix
