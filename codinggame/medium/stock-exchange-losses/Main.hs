import System.IO
import Control.Monad

data FoldStruct = FoldStruct {
        maxVal :: [Int], 
        looses :: [Int]
    } deriving (Show)

foldFct :: FoldStruct -> Int -> FoldStruct
foldFct foldStr@(FoldStruct maxs@(lastMax:rLast) looses@(lastL:rL)) val
        | val - lastMax < lastL = FoldStruct maxs (val - lastMax:rL) 
        | val > lastMax = FoldStruct (val:maxs) (0:looses)
        | otherwise = foldStr
foldFct (FoldStruct [] []) val = FoldStruct [val] [0]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE    
    
    contents <- getContents
    
    let times = map (\x -> read x ::Int) $ (tail . words) contents
    
    let folded = foldl foldFct (FoldStruct [] []) times
    
    -- Write answer to stdout
    putStrLn $ show $ (minimum . looses) folded
        
    return ()
