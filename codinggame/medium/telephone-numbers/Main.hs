import System.IO
import Control.Monad

import Data.Set (Set)
import qualified Data.Set as Set

type StrSet = Set String

strToPrefix :: String -> [String]
strToPrefix str = [p | n <- [1..length str], p <- [take n str] ]

putInSet :: StrSet -> String ->  StrSet
putInSet set str = Set.union set (Set.fromList (strToPrefix str))

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let n = read input_line :: Int
    
    tels <- replicateM n $ do
        input_line <- getLine
        let telephone = input_line :: String
        return telephone
    
    let set = foldl putInSet Set.empty tels
    
    print $ Set.size set
    
    return ()
