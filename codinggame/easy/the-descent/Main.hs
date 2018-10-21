import System.IO
import Control.Monad
import Data.List
import Data.Ord

maxi xs = maximumBy (comparing snd) (zip [0..] xs)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- The while loop represents the game.
    -- Each iteration represents a turn of the game
    -- where you are given inputs (the heights of the mountains)
    -- and where you have to print an output (the index of the mountain to fire on)
    -- The inputs you are given are automatically updated according to your last actions.
    
    loop

loop :: IO ()
loop = do
    
    heights <- replicateM 8 $ do
        input_line <- getLine
        let mountainh = read input_line :: Int -- represents the height of one mountain.
        return mountainh
    
    let maxElt = maxi(heights)
        index = fst(maxElt)
    
    putStrLn (show(index))
    
    loop
