import System.IO
import Control.Monad
import Data.List.Split
import Data.Ord
import Data.List

readFloatWithComma :: String -> Float
readFloatWithComma = read . sanitize
  where
    sanitize = map (\c -> if c == ',' then '.' else c)

data Point = Point { longitude :: Float,
                     latitude :: Float 
                   } deriving (Show)  

data Defibrilateur = Defibrilateur { numero :: Int,
                                     nom :: String,
                                     adresse :: String,
                                     telephone :: String,
                                     position :: Point} deriving (Show)


distance :: Point -> Point -> Float
distance a b = sqrt(x*x + y*y) * 6371
    where x = (longitude b - longitude a) * cos((latitude a + latitude b) / 2)
          y = latitude b - latitude a

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine

    let lon = readFloatWithComma (input_line :: String) * pi / 180
    
    input_line <- getLine
    let lat = readFloatWithComma (input_line :: String) * pi / 180

    -- Creation d'un point
    let pos = Point lon lat
    -- hPutStrLn stderr $ show pos
    
    input_line <- getLine
    let n = read input_line :: Int
    
    defibrilateurs <- replicateM n $ do
        defib <- getLine
        
        let items = splitOn ";" defib

        let indice = read (items!!0) :: Int
        let nom = items!!1
        let adresse = items!!2
        let telephone = items!!3
        let lon = readFloatWithComma(items!!4) * pi / 180
        let lat = readFloatWithComma(items!!5) * pi / 180
        let defibrilateur = Defibrilateur indice nom adresse telephone (Point lon lat)
        
        return defibrilateur
    
    -- hPutStrLn stderr $ show defibrilateurs
    let maxDist = minimumBy (comparing (\d -> distance pos (position d))) defibrilateurs
    -- hPutStrLn stderr $ show maxDist
    
    -- Write answer to stdout
    putStrLn (nom maxDist)
    
    return ()
