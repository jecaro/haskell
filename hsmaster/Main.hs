{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

import qualified Brick.Main                    as M
import qualified Brick.Types                   as T
import           Brick.Widgets.Core
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Edit            as E
import qualified Brick.AttrMap                 as A
import qualified Brick.Widgets.Dialog as D

import           Control.Monad
import           Control.Arrow                            ( (>>>) )
import           Data.List
import           Data.Maybe
import qualified Data.Text.Zipper              as Z
import qualified Graphics.Vty                  as V
import           Safe
import           System.Console.ANSI
import           System.Environment
import           System.IO
import           System.Random
import qualified Text.Read                     as TR

import           Game

import           Lens.Micro
import           Lens.Micro.TH


data Name = Prompt
          deriving (Ord, Show, Eq)

data State = State { _game   :: Game
                   , _editor :: E.Editor String Name }
makeLenses ''State

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
checkArgs (x : xs) = x == "-h" && checkArgs xs
checkArgs []       = True

-- Print simple usage
usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ progName ++ ": [-n 10]"
  putStrLn "-n 10\tSet the number of trials (default 10)"

-- Show the hint for a word
showHint :: Game -> String -> String
showHint game "" = "   "
showHint game guess = show f ++ " " ++ show s 
  where (f, s) = compute game guess

-- Rendering function
drawUI :: State -> [T.Widget Name]
drawUI state = [ui]
 where
  g        = state ^. game
  e        = E.renderEditor (str . unlines) True $ state ^. editor
  guesses' = reverse $ take (g ^. nbTrials) $ g ^. guesses ++ repeat ""
  fstCol   = map (intersperse ' ') guesses'
  hint     = map (showHint g) guesses'
  ui       = C.center $ hLimit 25 $ B.border $ vBox
    [ hBox
      [ padLeftRight 2 $ C.hCenter $ str $ unlines fstCol
      , vLimit (g ^. nbTrials) B.vBorder
      , padLeftRight 2 $ str $ unlines hint
      ]
    , B.hBorder
    , str "> " <+> e
    ]

-- End of game message
endMsg :: Status -> Maybe String
endMsg Won  = Just "You won !"
endMsg Lost = Just "You loose !"
endMsg _    = Nothing

-- Edit a zipper to write a message
showMsg :: Monoid a => String -> Z.TextZipper a -> Z.TextZipper a
showMsg msg = foldl (>>>) Z.clearZipper $ map Z.insertChar msg

-- Event handler
appEvent :: State -> T.BrickEvent Name e -> T.EventM Name (T.Next State)
-- Quit the game
appEvent state (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt state
-- Cheat mode, press h to show the secret number
appEvent state (T.VtyEvent (V.EvKey (V.KChar 'h') [])) =
  M.continue $ state & editor %~ E.applyEdit (showMsg (state ^. game . secret)) 
-- Main event handler
appEvent state (T.VtyEvent ev) = if status (state ^. game) /= Continue
  then M.halt state
  else do
    -- The new editor with event handled
    editor' <- E.handleEditorEvent ev (state ^. editor)
    -- Its content
    let guess = unwords $ E.getEditContents editor'
    -- We check the validity of the typed guess
    if not $ validPartialGuess (state ^. game) guess
      then M.continue state
      else
        -- Its length is still wrong
        if not $ validGuess (state ^. game) guess
        then M.continue $ state & editor .~ editor'
        else do
          -- Append guess to stack of guesses
          let game' = addGuess (state ^. game) guess
          -- Show the message if needed
          let zipper = case endMsg (status game') of
                Nothing  -> Z.clearZipper
                Just msg -> showMsg msg
          -- Carry on
          M.continue $ state & game .~ game' & editor %~ E.applyEdit zipper 

appEvent game _ = M.continue game

-- Main record for the brick application
theApp :: M.App State e Name
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
      let nbTrials  = fromMaybe 10 $ getNbTrials args 
      -- Game configuration
      let nbLetters = 4
      -- Secret word to guess
      gen <- getStdGen
      -- Init the game and start it
      let defaultEditor = E.editor Prompt (Just 1) ""
          game = draw (createGame nbTrials ['0'..'9']) 4 gen
      void $ M.defaultMain theApp (State game defaultEditor)
