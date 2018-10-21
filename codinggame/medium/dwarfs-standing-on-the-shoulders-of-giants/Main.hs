import System.IO
import Control.Monad

import Data.List
import Data.Ord
import Data.Tree

import Data.Graph.Inductive
import Data.Graph.Inductive.Query.SP

-- 5
-- 1 2
-- 2 3
-- 2 4
-- 3 4
-- 4 5

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

depth :: Graph gr => gr a b -> Node -> Int
depth graph n = length (levels tree)
    where 
        tree = dff [n] graph !! 0

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let n = read input_line :: Int -- the number of relationships of influence
    
    arcs <- replicateM n $ do
        input_line <- getLine
        let input = words input_line
        let x = read (input!!0) :: Int -- a relationship of influence between two people (x influences y)
        let y = read (input!!1) :: Int
        return (x, y)
    -- hPutStrLn stderr $ show arcs
    
    -- les noeuds
    let noeuds = rmdups $ concatMap (\(x, y) -> [x, y]) arcs
    --hPutStrLn stderr $ show noeuds
    
    -- creation du graph
    let graph = emap (\x -> 1) (mkUGraph noeuds arcs :: Gr () ())
    --hPutStrLn stderr $ show graph
    
    -- recuperation des racines
    let roots = filter (\x -> indeg graph x == 0) (nodes graph)
    -- hPutStrLn stderr $ show roots
    
    -- calcul des profondeurs pour chaque racine
    let depths = map (\x -> depth graph x) roots
    --hPutStrLn stderr $ show depths
        
    -- on renvoie le max
    putStrLn $ show $ (maximum depths)
    return ()
