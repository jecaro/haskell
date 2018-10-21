import System.IO
import Control.Monad

dirX x = if x > 0 then 'E' else 'W'

dirY y = if y > 0 then 'S' else 'N'

direction n dir = replicate (abs n) [dir n]

zipWithPadding :: [a] -> [a] -> a -> [(a, a)]
zipWithPadding (x:xs) (y:ys) pad = (x, y) : zipWithPadding xs ys pad
zipWithPadding xs [] pad = zip xs (repeat pad)
zipWithPadding [] ys pad = zip (repeat pad) ys

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    -- ---
    -- Hint: You can use the debug stream to print initialTX and initialTY, if Thor seems not follow your orders.
    
    input_line <- getLine
    let input = words input_line
    let lightx = read (input!!0) :: Int -- the X position of the light of power
    let lighty = read (input!!1) :: Int -- the Y position of the light of power
    let initialtx = read (input!!2) :: Int -- Thor's starting X position
    let initialty = read (input!!3) :: Int -- Thor's starting Y position
  
   -- hPutStrLn stderr $ show (dirY (lightx - initialtx))
    
    let xMove = direction (lightx - initialtx) (dirX)
    let yMove = direction (lighty - initialty) (dirY)
    
    -- hPutStrLn stderr $ show xMove
    -- hPutStrLn stderr $ show yMove

    let directionsList = zipWithPadding yMove xMove ""
    let directionsStr = map (\n -> fst(n) ++ snd(n)) directionsList

    -- hPutStrLn stderr $ show directionsStr
    
    loop directionsStr

loop :: [String] -> IO()
loop moves = do
    input_line <- getLine
    let remainingturns = read input_line :: Int -- The remaining amount of turns Thor can move. Do not remove this line.
    
    -- A single line providing the move to be made: N NE E SE S SW W or NW
    
    putStrLn (head moves)
    
    loop (tail moves)
