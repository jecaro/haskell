import System.IO
import Control.Monad
import Data.List
import Data.Maybe

noeuds :: [[Bool]] -> Int -> Int -> [(Int, Int)]
noeuds matrice width height = [(x,y) 
    | x <- [0..width - 1], y <- [0..height - 1], matrice !! y !! x]


right :: [[Bool]] -> Int -> (Int, Int) -> (Int, Int)
right matrice width (x,y) = case [x | x <- [x+1..width-1], matrice !! y !! x] of
    [] -> (-1, -1)
    (x1:_) -> (x1, y)

col n = map (!! n)

bottom :: [[Bool]] -> Int -> (Int, Int) -> (Int, Int)
bottom matrice height (x,y) = case [y | y <- [y+1..height-1], matrice !! y !! x] of
    [] -> (-1, -1)
    (y1:_) -> (x, y1)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Don't let the machines win. You are humanity's last hope...
    
    input_line <- getLine
    let width = read input_line :: Int -- the number of cells on the X axis
    input_line <- getLine
    let height = read input_line :: Int -- the number of cells on the Y axis
    
    matrice <- replicateM height $ do
        line <- getLine
        -- width characters, each either 0 or .
        return $ map (\x -> x == '0') line
    
    hPutStrLn stderr $ show $ matrice

    -- Calcul des noeuds True
    let lesNoeuds = noeuds matrice width height

    hPutStrLn stderr $ show lesNoeuds
    --let test = right matrice (2, 0)
    --hPutStrLn stderr $ show test

    -- Une ligne solution
    let soluce matrice unNoeud = [unNoeud, right matrice width unNoeud, bottom matrice height unNoeud]
    
    -- Affichage d'une coordonnee
    let prtCoord n = show (fst n) ++ " " ++ show (snd n)
    
    -- Affichage d'une ligne solution
    let prtSoluce n = intercalate " " $ map prtCoord n
    
    -- Calcul de toutes les solutions
    let answer = map (\n -> prtSoluce (soluce matrice n)) lesNoeuds
    
    mapM_ putStrLn answer
    -- hPutStrLn stderr $ show answer

    return ()
