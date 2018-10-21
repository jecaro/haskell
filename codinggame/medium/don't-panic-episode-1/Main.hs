import System.IO
import Control.Monad
import Data.List
import Data.Ord
import Data.Tuple

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let input = words input_line
    let nbfloors = read (input!!0) :: Int -- number of floors
    let width = read (input!!1) :: Int -- width of the area
    let nbrounds = read (input!!2) :: Int -- maximum number of rounds
    let exitfloor = read (input!!3) :: Int -- floor on which the exit is found
    let exitpos = read (input!!4) :: Int -- position of the exit on its floor
    let nbtotalclones = read (input!!5) :: Int -- number of generated clones
    let nbadditionalelevators = read (input!!6) :: Int -- ignore (always zero)
    let nbelevators = read (input!!7) :: Int -- number of elevators
    
    -- A chaque etage position sortie
    
    elevators <- replicateM nbelevators $ do
        input_line <- getLine
        let input = words input_line
        let elevatorfloor = read (input!!0) :: Int -- floor on which this elevator is found
        let elevatorpos = read (input!!1) :: Int -- position of the elevator on its floor
        return (elevatorfloor, elevatorpos)
    
    let exits = elevators ++ [(exitfloor, exitpos)]
    
    let sortedExits = sortBy (comparing fst) exits
    
    loop sortedExits

loop :: [(Int, Int)] -> IO ()
loop exits = do
    input_line <- getLine
    let input = words input_line
    let clonefloor = read (input!!0) :: Int -- floor of the leading clone
    let clonepos = read (input!!1) :: Int -- position of the leading clone on its floor
    let direction = input!!2 -- direction of the leading clone: LEFT or RIGHT
    
    let floor 
            | not $ null exits = fst $ head exits
            | otherwise = -1
    let pos 
            | not $ null exits = snd $ head exits
            | otherwise = -1

    let dir = clonepos - pos
    let rightDir = dir > 0 && direction == "LEFT" || 
                   dir < 0 && direction == "RIGHT"
    let rightFloor = floor == clonefloor
    
    --hPutStrLn stderr $ show rightDir
    --hPutStrLn stderr $ show rightFloor
    
    let action 
            | rightFloor && rightDir = "WAIT"
            | rightFloor && not rightDir = "BLOCK"
            | otherwise = "WAIT"
   
    let newExits 
            | rightFloor = if null exits then [] else tail exits
            | otherwise = exits
    
    putStrLn action
    
    loop newExits
