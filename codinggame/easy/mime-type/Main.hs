import System.IO
import Control.Monad
import System.FilePath
import Data.List
import Data.Char

-- import qualified Data.HashMap.Lazy as Map 
-- import qualified Data.Map as Map  
import qualified Data.Map as Map  

type MapStrStr = Map.Map String String

readMap :: Int -> IO (MapStrStr)
readMap n = 
    let 
        readMapRec aMap 0 = return aMap
        readMapRec aMap n = do
            input_line <- getLine
            let input = words input_line
                ext = map toLower $ input!!0 -- file extension
                mt = input!!1 -- MIME type.
                newMap = Map.insert ext mt aMap
            readMapRec newMap (n - 1)
    in 
        readMapRec Map.empty n
            
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let n = read input_line :: Int -- Number of elements which make up the association table.
    input_line <- getLine
    let q = read input_line :: Int -- Number Q of file names to be analyzed.
    
    mimes <- readMap n
    
    replicateM q $ do
        fname <- getLine 
        let extWithDot = map toLower $ takeExtension fname
            extWithoutDot = if null extWithDot then [] else tail extWithDot

            elt = Map.lookup extWithoutDot mimes
        
        case elt of 
            Just value -> putStrLn value
            Nothing -> putStrLn "UNKNOWN"

    -- mapM_ putStrLn answers    
    -- hPutStrLn stderr "Debug messages..."
    
    -- For each of the Q filenames, display on a line the corresponding MIME type. If there is no corresponding type, then display UNKNOWN.
    
    return ()
