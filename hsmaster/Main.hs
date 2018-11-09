{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

import           Control.Monad
import           Control.Monad.State
import           Data.List
import           Data.Maybe
import           Safe
import           System.Console.ANSI
import           System.Environment
import           System.IO
import           System.Random
import qualified Text.Read                     as TR

import           Lens.Micro
import           Lens.Micro.TH
import qualified Graphics.Vty                  as V

import qualified Brick.Main                    as M
import qualified Brick.Types                   as T
import           Brick.Widgets.Core
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Edit            as E
import qualified Brick.AttrMap                 as A

import qualified Data.Text.Zipper              as Z
import           Control.Arrow                            ( (>>>) )

-- TODO 
-- add a message end of game
-- try to add reader monad again

data Name = Prompt
          deriving (Ord, Show, Eq)

data St = St {
  _editor   :: E.Editor String Name,
  _guesses  :: [String],
  _nbTrials :: Int,
  _values   :: String,
  _secret   :: String  } deriving (Show)
makeLenses ''St

-- Status of the game
data Status = Won | Lost | Continue deriving (Eq)

-- Take a value at a specific index in a list
-- return this value along the remaining list
pick :: [a] -> Int -> (a, [a])
pick list ind = (list !! ind, start ++ end)
 where
  start = take ind list
  end   = drop (succ ind) list

-- Shuffle a list 
shuffle :: RandomGen g => [a] -> State g [a]
shuffle []   = return []
shuffle list = do
  ind <- state $ randomR (0, length list - 1)
  let (val, list') = pick list ind
  fmap (val :) (shuffle list')

-- Compute the result of the guess
compute :: Eq a => [a] -> [a] -> (Int, Int)
compute secret guess = (goodSpot, good - goodSpot)
 where
  goodSpot = length . filter (uncurry (==)) $ zip secret guess
  good     = length $ filter (`elem` secret) guess

-- Check if a list contains duplicates elements
hasDuplicate :: (Eq a, Ord a) => [a] -> Bool
hasDuplicate w = any (\x -> length x >= 2) $ group $ sort w

-- Return the validity of the guess
-- - the right length
-- - no repeated letters
-- - correct values
validPartialGuess :: Int -> String -> String -> Bool
validPartialGuess n values guess = (length guess <= n)
  && not (hasDuplicate guess)
  && all (`elem` values) guess

-- Get the number of trials from arg list
getNbTrials :: [String] -> Maybe Int
getNbTrials args = do
  ind <- "-n" `elemIndex` args
  el  <- atMay args $ succ ind
  TR.readMaybe el

-- Check the validity of the arg list
checkArgs :: [String] -> Bool
checkArgs ("-n" : x : xs) =
  isJust (TR.readMaybe x :: Maybe Int) && checkArgs xs
checkArgs (x : xs) = (x == "-d" || x == "-h") && checkArgs xs
checkArgs []       = True

-- Print simple usage
usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ progName ++ ": [-d] [-n 10]"
  putStrLn "-d\tDebug mode"
  putStrLn "-n 10\tSet the number of trials (default 10)"

showStl :: String -> String -> String
showStl secret "" = "   "
showStl secret guess = show f ++ " " ++ show s 
  where (f, s) = compute secret guess

-- Rendering function
drawUI :: St -> [T.Widget Name]
drawUI st = [ui]
 where
  e        = E.renderEditor (str . unlines) True (st ^. editor)
  guesses' = reverse $ take (st^.nbTrials) $ st ^. guesses ++ repeat ""
  fstCol   = map (intersperse ' ') guesses'
  slt      = map (showStl (st ^. secret)) guesses'
  ui       = C.center $ hLimit 25 $ B.border $ vBox
    [ hBox
      [ padLeftRight 2 $ C.hCenter $ str $ unlines fstCol
      , vLimit (st^.nbTrials) B.vBorder
      , padLeftRight 2 $ str $ unlines slt
      ]
    , B.hBorder
    , str "> " <+> e
    ]

endOfGame :: St -> Bool
endOfGame st 
  -- no guesses
  | null $ st ^. guesses = False
  -- win
  | last (st ^. guesses) == (st ^. secret) = True
  -- loose
  | length (st ^. guesses) == (st ^. nbTrials) = True
  -- anything else
  | otherwise = False

-- Event handler
appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent st (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st (T.VtyEvent ev                 ) = 
  if endOfGame st 
    then M.halt st
    else do
      -- The new editor with event handled
      newEditor <- E.handleEditorEvent ev (st ^. editor)
      -- Its content
      let guess = unwords $ E.getEditContents newEditor
      -- We check the validity of the typed guess
      if not $ validPartialGuess (length $ st ^. secret) (st ^. values) guess
        then M.continue st
        else
          -- Its length is still wrong
          if length guess < length (st ^. secret)
          then M.continue $ st & editor .~ newEditor
          else 
            M.continue
            $  st
            &  editor %~ E.applyEdit Z.clearZipper 
            &  guesses %~ (guess:)

appEvent st _ = M.continue st

-- Initial state
defaultState :: St
defaultState = St { _editor = E.editor Prompt (Just 1) ""
                  , _guesses = []
                  , _nbTrials = 3
                  , _values = ['0'..'9']
                  , _secret = "1324" } 

-- Main record for the brick application
theApp :: M.App St e Name
theApp =
    M.App { M.appDraw         = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent  = appEvent
          , M.appStartEvent   = return
          , M.appAttrMap      = const $ A.attrMap V.defAttr [] 
          }

-- Main function
main :: IO ()
main = do
  -- Arguments initialization
  args <- getArgs 
  if not (checkArgs args) || "-h" `elem` args
    then usage
    else do
      let debug     = "-d" `elem` args
      let nbTrials'  = fromMaybe 10 $ getNbTrials args 
      -- Game configuration
      let nbLetters = 4
      -- Secret word to guess
      gen <- getStdGen
      let secret' = take nbLetters $ evalState (shuffle $ defaultState^.values) gen 
      -- Show secret word in debug mode
      when debug $ putStrLn secret' >> void getChar
      let initialState = defaultState & nbTrials .~ nbTrials' & secret .~ secret'
      void $ M.defaultMain theApp initialState
