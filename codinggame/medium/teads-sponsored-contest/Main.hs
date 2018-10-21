import System.IO
import Control.Monad

import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List
import Data.Ord

type Node = Int
type SetNode = Set Node
type MapNode = Map Node SetNode

listToTuple :: [Node] -> [(Node, Node)]
listToTuple (n1:n2:r) =  [(n1, n2)] ++ listToTuple r
listToTuple _         =  []

fillUpGraph :: (Node, Node) -> MapNode -> MapNode
fillUpGraph (n1, n2) mapNode = mapWithN1N2
    where
        mapWithN1 = Map.insertWith Set.union n1 (Set.singleton n2) mapNode
        mapWithN1N2 = Map.insertWith Set.union n2 (Set.singleton n1) mapWithN1

removeNodeFromAdj :: Node -> (Node, MapNode) -> (Node, MapNode)
removeNodeFromAdj node (key, graph) = (key, newGraph)
    where
        newGraph = Map.adjust (Set.delete key) node graph

deleteUnary' :: Node -> SetNode -> MapNode -> MapNode
deleteUnary' node adjs graph 
        | Set.size adjs == 1 = 
            let graphWithoutNode = Map.delete node graph
                graphUpdate = snd $ Set.foldr removeNodeFromAdj (node, graphWithoutNode) adjs
            in graphUpdate
        | otherwise = graph
        
deleteUnary :: MapNode -> MapNode        
deleteUnary graph = Map.foldrWithKey deleteUnary' graph graph
                
solveRec :: Int -> MapNode -> (Int, MapNode)
solveRec level graph 
        | Map.size graph <= 1 = (level, graph)
        | otherwise = solveRec (level + 1) $ deleteUnary graph
        
solve :: MapNode -> Int
solve graph = fst $ solveRec 0 graph
        
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let n = read input_line :: Int -- the number of adjacency relations

    content <- getContents
    let arcs = listToTuple . map (read ::String->Int) $ words content
    --hPutStrLn stderr $ show arcs

    let graph = foldr fillUpGraph Map.empty arcs
    --hPutStrLn stderr $ show graph
    
    let answer = solve graph
    
    print answer
        
    return ()
