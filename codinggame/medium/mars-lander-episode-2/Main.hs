import System.IO
import Control.Monad
import Data.List
import Data.Ord

data Position = Position {x :: Int, y ::Int} deriving (Show)
data Range = Range {x1 :: Int, x2 :: Int} deriving (Show)
data Flat = Flat {range :: Range, height :: Int} deriving (Show)

flatPart' :: [Position] -> [Flat] -> [Flat]
flatPart' [_] flats = flats
flatPart' ((Position x1 y1):(Position x2 y2):rest) flats = if y1 == y2 
    then flatPart' rest (flats ++ [Flat (Range x1 x2) y1])
    else flatPart' ([Position x2 y2] ++ rest) flats

flatPart :: [Position] -> [Flat]
flatPart points = flatPart' points []

distHorFlatPos :: Position -> Flat -> Double
distHorFlatPos (Position x y) (Flat (Range x1 x2) heigh) = 
    abs((fromIntegral x) - (fromIntegral (x1-x2)) / 2.0)

closest :: [Flat] -> Position -> Flat
closest flats pos = sortedFlats !! 0
    where sortedFlats = sortBy (comparing (distHorFlatPos pos)) flats

constraint :: Int -> Int
constraint angle 
        | angle >= -180 && angle <= 180 = angle
        | angle < -180 = constraint $ angle + 360
        | angle > 180 = constraint $ angle - 360

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let surfacen = read input_line :: Int -- the number of points used to draw the surface of Mars.
    
    land <- replicateM surfacen $ do
        input_line <- getLine
        let input = words input_line
        let landx = read (input!!0) :: Int -- X coordinate of a surface point. (0 to 6999)
        let landy = read (input!!1) :: Int -- Y coordinate of a surface point. By linking all the points together in a sequential fashion, you form the surface of Mars.
        return (Position landx landy)
    
    loop land

loop :: [Position] -> IO ()
loop land = do
    input_line <- getLine
    let input = words input_line
    let x = read (input!!0) :: Int
    let y = read (input!!1) :: Int
    let hspeed = read (input!!2) :: Int -- the horizontal speed (in m/s), can be negative.
    let vspeed = read (input!!3) :: Int -- the vertical speed (in m/s), can be negative.
    let fuel = read (input!!4) :: Int -- the quantity of remaining fuel in liters.
    let rotate = read (input!!5) :: Int -- the rotation angle in degrees (-90 to 90).
    let power = read (input!!6) :: Int -- the thrust power (0 to 4).
    
    let pos = Position x y 
    
    let flats = flatPart land
    let flat = closest flats pos
    
    let halpha = 0.1 * (fromIntegral $ abs(hspeed * hspeed))
    
    let x1Flat = (x1 $ range flat) - (round halpha)
    let x2Flat = (x2 $ range flat) + (round halpha)
    
    let diffHeight = y - (height flat)
    
    let sens
            | x > x2Flat = 1 
            | x < x1Flat = -1 
            | otherwise = 0

    let inRange = x >= x1Flat && x <= x2Flat
    
    let rotateCommand 
            | not inRange && (abs hspeed < 50) = rotate + sens * 15
            | otherwise = constraint $ 90 + (round ((atan2 (fromIntegral vspeed) (fromIntegral hspeed)) * 180.0 / pi) :: Int) 

    let normSpeed = sqrt((fromIntegral $ hspeed * hspeed) + (fromIntegral $ vspeed * vspeed))

    let powerCommand 
            | inRange && hspeed == 0 && (abs vspeed) < 30 = max 0 $ power - 1
            | otherwise = 4
    
    let alphaAngle = min 1.0 $ (fromIntegral diffHeight) / 500.0
    
    let maxAngle 
            | not inRange = 20
            | otherwise = round $ 40.0 * alphaAngle
    
    let rotate  
            | rotateCommand > maxAngle = maxAngle
            | rotateCommand < - maxAngle = - maxAngle
            | diffHeight < 100 = 0
            | otherwise = rotateCommand
    
    putStrLn $ (show rotate) ++ " " ++ (show powerCommand)
    
    loop land
