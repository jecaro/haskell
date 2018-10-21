import System.IO
import Control.Monad

data Window = Window {
    top :: Int, 
    bottom :: Int, 
    left :: Int, 
    right :: Int} deriving (Show)

fromPosDir :: (Int, Int) -> String -> Window
fromPosDir (x, y) "U" = Window 0 (y - 1) x x 
fromPosDir (x, y) "D" = Window (y + 1) maxBound x x 
fromPosDir (x, y) "R" = Window y y (x + 1) maxBound 
fromPosDir (x, y) "L" = Window y y 0 (x - 1)  
fromPosDir (x, y) "UR" = Window 0 (y - 1) (x + 1) maxBound
fromPosDir (x, y) "UL" = Window 0 (y - 1) 0 (x - 1)  
fromPosDir (x, y) "DR" = Window (y + 1) maxBound (x + 1) maxBound
fromPosDir (x, y) "DL" = Window (y + 1) maxBound 0 (x - 1)  

inter :: Window -> Window -> Window
inter (Window t1 b1 l1 r1) (Window t2 b2 l2 r2) = Window 
    (max t1 t2) (min b1 b2) (max l1 l2) (min r1 r2)


middle :: Window -> (Int, Int)
middle (Window t b l r) = ((l + r) `div` 2, (t + b) `div` 2)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let input = words input_line
    let w = read (input!!0) :: Int -- width of the building.
    let h = read (input!!1) :: Int -- height of the building.
    input_line <- getLine
    let n = read input_line :: Int -- maximum number of turns before game over.
    input_line <- getLine
    let input = words input_line
    let x0 = read (input!!0) :: Int
    let y0 = read (input!!1) :: Int
    
    loop (x0, y0) $ Window 0 h 0 w

loop :: (Int, Int) -> Window -> IO ()
loop pos window = do
    
    -- hPutStrLn stderr $ show pos
    -- hPutStrLn stderr $ show window

    input_line <- getLine
    let bombdir = input_line :: String -- the direction of the bombs from batman's current location (U, UR, R, DR, D, DL, L or UL)
    
    -- hPutStrLn stderr bombdir
    
    let winDir = fromPosDir pos bombdir
    -- hPutStrLn stderr $ show winDir
    
    let winInter = inter window winDir
    -- hPutStrLn stderr $ show winInter
    
    let newPos = middle winInter
    -- hPutStrLn stderr "Debug messages..."
    
    -- the location of the next window Batman should jump to.
    
    putStrLn $ (show $ fst newPos) ++ " " ++ (show $ snd newPos)
    
    loop newPos winInter
