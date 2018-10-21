import System.IO
import Control.Monad
import Data.Array.IArray

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Map as Map
import Data.List

-- map -> modifiable
-- bender -> inverse, casseur, direction Eq
-- chemin -> position + liste de bender

data Direction = SOUTH | EAST | NORTH | WEST deriving (Show, Eq, Ord)

data Bender = Bender { inverse :: Bool
                     , breaker :: Bool
                     , direction :: Direction } deriving (Show, Eq, Ord)

type Position = (Int, Int)

type MapWorld = Array (Int, Int) Char

type BenderStates = Set Bender
type MapPosStates = Map.Map Position BenderStates

data State = State { world     :: MapWorld
                   , position  :: Position
                   , bender    :: Bender 
                   , posStates :: MapPosStates
                   , loop      :: Bool
                   , dirs      :: [Direction]} deriving (Show)

updateBender :: Bender -> Direction -> Char -> Bender
updateBender (Bender i b _) d 'B' = Bender i (not b) d
updateBender (Bender i b _) d 'I' = Bender (not i) b d
updateBender (Bender i b _) d _ = Bender i b d

teleporte :: MapWorld -> Position -> Position
teleporte w p = case findTele w of
    Just (t1, t2) -> if t1 == p 
        then t2 
        else if t2 == p
            then t1
            else p
    Nothing -> p

advance :: State -> State
advance (State w pos bend poss l dirs) = 
        -- On trouve la prochaine direction correcte
    let newDir = validDir w bend pos
        -- Mise a jour de la liste des directions
        newDirs = (dirs ++ [newDir])
        -- Calcul de la nouvelle position
        newPos = nextPosition newDir pos
        -- La nouvelle case
        c = w ! newPos
        -- Modificateur de trajectoire
        modifiedDir = case c of
            'E' -> EAST
            'W' -> WEST
            'N' -> NORTH
            'S' -> SOUTH
            _ -> newDir
        -- Teleportation
        telePos = teleporte w newPos
        -- Le nouveau bender avec dir, i, et b mis a jour
        newBend = updateBender bend modifiedDir c 
        -- Mise a jour de la matrice
        (newW, modif) = case c of
            'X' -> (w // [(newPos, ' ')], True)
            _ -> (w, False)
        -- Test d'une boucle
        newLoop = case Map.lookup telePos poss of
            Just benders -> Set.member newBend benders
            Nothing -> False
        -- Ajout de la position courante dans les etats
        newPoss = if modif 
            then Map.empty
            else Map.insertWith (Set.union) telePos (Set.singleton newBend) poss
        -- Mise a jour de l'etat
        in State newW telePos newBend newPoss newLoop newDirs
        
advanceRec :: State -> State
advanceRec state
        | world state ! (position state) == '$' = state
        | loop state = state
        | otherwise = advanceRec $ advance state

offset :: Direction -> Position
offset NORTH = (-1, 0)
offset SOUTH = (1, 0)
offset EAST  = (0, 1)
offset WEST  = (0, -1)

testDir :: MapWorld -> Bool -> Position -> Direction -> Bool
testDir world b pos dir = 
    let newPos = nextPosition dir pos
        c = world ! newPos
    in c /= '#' && not ((c == 'X') && not b)
        

validDir :: MapWorld -> Bender -> Position -> Direction
validDir world (Bender i b d) p = 
    -- Toutes les directions a tester courante + les autres
    let dirsToTry = d:(priorities' i)
    -- On trouve la premiere qui marche ou on prend la derniere
    in case find (testDir world b p) dirsToTry of
        Just value -> value
        Nothing -> last dirsToTry
        
nextPosition :: Direction -> Position -> Position
nextPosition dir (lPos, cPos) = (lDir + lPos, cDir + cPos)
    where (lDir, cDir) = offset dir

priorities :: [Direction]
priorities = [SOUTH, EAST, NORTH, WEST]

priorities' :: Bool -> [Direction]
priorities' i = if not i 
    then priorities
    else reverse priorities

printArray :: MapWorld -> String
printArray arr = unlines [unwords [[arr ! (x, y)] | y <- [c0..c1]] | x <- [l0..l1]]
    where ((l0, c0), (l1,c1)) = bounds arr

findBender :: MapWorld -> (MapWorld, Position)
findBender wWith = (wWithout, ind)
    where 
        ind = [e | e <- range (bounds wWith), wWith ! e == '@'] !! 0
        wWithout = wWith // [(ind, ' ')]

findTele :: MapWorld -> Maybe (Position, Position)
findTele world = case teles of
    [x, y] -> Just (x, y)
    _ -> Nothing
    where teles = [e | e <- range (bounds world), world ! e == 'T']
        
debugState :: State -> Int -> IO ()
debugState state@(State w pos bend poss l dirs) level = do
    --hPutStrLn stderr $ show w
    hPutStrLn stderr "debug state"
    -- La matrice
    let wToPrint = w // [(pos, '@')]
    hPutStrLn stderr$ printArray wToPrint
    -- La position
    hPutStrLn stderr $ "pos " ++ show pos
    -- La direction courante
    let dir = if null dirs then " " else show $ last dirs
    hPutStrLn stderr $ "dir " ++ show dir
    
    --hPutStrLn stderr $ show bend

    -- Recursion
    when (level /= 0) $ do
        debugState (advance state) (level - 1)
        
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let input = words input_line
    let l = read (input!!0) :: Int
    let c = read (input!!1) :: Int
    
    worldStr <- replicateM l $ do
        row <- getLine
        return row    
    --hPutStrLn stderr $ show worldStr
    
    let wWithBender = array ((0, 0), (l - 1, c - 1)) [((x, y), worldStr !! x !! y) | x <- [0..l - 1], y <- [0..c - 1]] :: MapWorld
    --hPutStrLn stderr $ printArray wWithBender
    
    let (wWithout, pos) = findBender wWithBender
    --hPutStrLn stderr $ show pos
    --hPutStrLn stderr $ printArray wWithout

    let bender = (Bender False False SOUTH)
    let state = State wWithout pos bender Map.empty False []
    --debugState state 5
    
    let endState = advanceRec state
    --debugState endState 0
    
    -- Write answer to stdout
    if loop endState 
        then putStrLn "LOOP"
        else putStr $ unlines $ map show $ dirs endState

    return ()
