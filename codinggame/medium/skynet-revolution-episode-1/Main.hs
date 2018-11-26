import System.IO

import Control.Monad

import Data.Graph.Inductive
import Data.Graph.Inductive.Query.SP

import Data.List
import Data.Maybe
import Data.Ord
import Data.Tuple

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let input = words input_line
    let n = read (input!!0) :: Int -- the total number of nodes in the level, including the gateways
    let l = read (input!!1) :: Int -- the number of links
    let e = read (input!!2) :: Int -- the number of exit gateways
    
    arcs <- replicateM l $ do
        input_line <- getLine
        let input = words input_line
        let n1 = read (input!!0) :: Int -- N1 and N2 defines a link between these nodes
        let n2 = read (input!!1) :: Int
        return (n1, n2)
    
    gates <- replicateM e $ do
        input_line <- getLine
        let ei = read input_line :: Int -- the index of a gateway node
        return ei
            
    loop n arcs gates

loop :: Int -> [(Int, Int)] -> [Int] -> IO ()
loop n arcs gates = do
    input_line <- getLine
    let si = read input_line :: Int -- The index of the node on which the Skynet agent is positioned this turn

    -- Un graphe pondere a 1 et undirected
    let graph = emap (\x -> 1) $ undir (mkUGraph [0..n-1] arcs :: Gr () ())

    -- Les chemins pour aller vers les gates tries par taille
    let paths = sortBy (comparing length) $ mapMaybe (\x -> sp si x graph) gates

    let soluceList = take 2 $ paths !! 0
    let soluce = (soluceList!!0, soluceList!!1)
    
    -- hPutStrLn stderr $ show si
    -- hPutStrLn stderr $ show gates
    -- hPutStrLn stderr $ show graph
    -- hPutStrLn stderr $ show paths
    
    putStrLn $ show (fst soluce) ++ " " ++ show (snd soluce)
    
    let newArcs = delete soluce arcs
    
    loop n newArcs gates