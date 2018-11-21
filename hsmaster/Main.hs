{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

import qualified Brick.AttrMap                 as A
import qualified Brick.Main                    as M
import qualified Brick.Types                   as T
import           Brick.Util (on, bg)
import           Brick.Widgets.Core
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Dialog          as D
import qualified Brick.Widgets.Edit            as E

import           Control.Monad
import           Control.Arrow                            ( (>>>) )
import           Data.List
import           Data.Maybe
import qualified Data.Text.Zipper              as Z
import qualified Graphics.Vty                  as V
import           Lens.Micro
import           Lens.Micro.TH
import           Safe
import           System.Console.ANSI
import           System.Environment
import           System.IO
import           System.Random
import           System.Random.Shuffle
import qualified Text.Read                     as TR

import           Game

-- TODO
-- Replace by microlens-platform
-- Move command line in a separate module
-- Esc give up
-- Implement loop
-- h to show the secret word

data Name = Prompt
          deriving (Ord, Show, Eq)

data YesNo = Yes | No deriving Show

data Widget = Editor (E.Editor String Name) | Dialog (D.Dialog YesNo)

data State = State { _game   :: Game
                   , _editor :: E.Editor String Name 
                   , _yesNo  :: Maybe (D.Dialog YesNo)
                   }
makeLenses ''State

-- Get the number of trials from arg list
getNbTrialsFromArgs :: [String] -> Maybe Int
getNbTrialsFromArgs args = do
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

-- End of game message
endMsg :: Status -> Maybe String
endMsg Won  = Just "You won !"
endMsg Lost = Just "You loose !"
endMsg _    = Nothing

-- Create the yes no dialog
yesNoDialog :: D.Dialog YesNo
yesNoDialog = D.dialog Nothing (Just (0, choices)) 30
  where choices = [("Yes", Yes), ("No", No)]

-- Draw the dialog
drawDialog :: State -> T.Widget Name
drawDialog State { _yesNo = Just yn, _game = game } =
  case endMsg $ status game of
    Nothing      -> emptyWidget
    Just endStr  -> D.renderDialog yn $ C.hCenter $ padAll 1 
                    $ str $ endStr ++ " Another game ?"
drawDialog _ = emptyWidget

-- Draw the main widget
drawMain :: State -> T.Widget Name
drawMain State { _game = g, _editor = e } = 
  C.center $ hLimit 25 $ B.border $ vBox
  [ hBox
    [ padLeftRight 2 $ C.hCenter $ str $ unlines fstCol
    , vLimit nbTrials B.vBorder
    , padLeftRight 2 $ str $ unlines sndCol
    ]
  , B.hBorder
  , str "> " <+> re
  ]
  where
    nbTrials   = g ^. getNbTrials
    re         = E.renderEditor (str . unlines) True e
    guesses    = reverse $ take nbTrials $ g ^. getGuesses ++ repeat ""
    fstCol     = map (intersperse ' ') guesses
    sndCol     = map (showHint g) guesses

-- Rendering function
drawUI :: State -> [T.Widget Name]
drawUI state = [drawDialog state, drawMain state]

-- Handle the dialog events
handleDialogEvent :: State -> T.BrickEvent Name e -> T.EventM Name (T.Next State)
handleDialogEvent state@State { _yesNo = (Just yn) } (T.VtyEvent ev) = do
  -- Handle event
  yn' <- D.handleDialogEvent ev yn

  -- Toggle no if esc has been pressed
  let toggle = if ev == V.EvKey V.KEsc [] then no else id
      state' = state & yesNo ?~ toggle yn'

  -- Enter or esc stop the event loop
  if ev == V.EvKey V.KEnter [] || ev == V.EvKey V.KEsc [] 
  then M.halt state'
  else M.continue state'

-- The dialog is not visible, this should not happen
handleDialogEvent state _ = M.continue state

-- Return the yes no dialog if the current game is finished
showYesNoIfNeeded :: State -> Maybe (D.Dialog YesNo)
showYesNoIfNeeded state | status (state ^. game) == Continue = Nothing
                        | otherwise                          = Just yesNoDialog

-- Handle the editor events
handleEditorEvent :: State -> T.BrickEvent Name e -> T.EventM Name (T.Next State)
handleEditorEvent state (T.VtyEvent ev) = do
  -- The new editor with event handled
  editor' <- E.handleEditorEvent ev (state ^. editor)
  -- Its content
  let guess = unwords $ E.getEditContents editor'
  -- We check the validity of the typed guess
  if not $ validPartialGuess (state ^. game) guess
    then M.continue state
    else
      let state' = if not $ validGuess (state ^. game) guess
                   -- The guess is not long enough, carry on
                   then state & editor .~ editor'
                   -- The guess is ok, append it to the guess list
                   -- and reset the editor
                   else state & editor %~ E.applyEdit Z.clearZipper 
                              & game %~ addGuess guess
      in  M.continue $ state' & yesNo .~ showYesNoIfNeeded state'
handleEditorEvent state _ = M.continue state

no :: D.Dialog YesNo -> D.Dialog YesNo
no d = d & D.dialogSelectedIndexL ?~ 1

dialogVisible :: State -> Bool
dialogVisible s = isJust (s ^. yesNo)

-- Return the current widget
frontWidget :: State -> Widget
frontWidget state | dialogVisible state = Dialog $ fromJust $ state ^. yesNo
                  | otherwise           = Editor $ state ^. editor

-- Event dispatcher
handleEvent :: Widget -> State -> T.BrickEvent Name e -> T.EventM Name (T.Next State)
handleEvent (Dialog d) = handleDialogEvent
handleEvent (Editor e) = handleEditorEvent

-- Event handler
appEvent :: State -> T.BrickEvent Name e -> T.EventM Name (T.Next State)
-- Quit the game
appEvent state (T.VtyEvent (V.EvKey V.KEsc [])) = 
  M.halt $ state & yesNo ?~ no yesNoDialog
-- Main event handler
appEvent state ev = handleEvent (frontWidget state) state ev

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.buttonSelectedAttr, bg V.black)
    ]

-- Main record for the brick application
theApp :: M.App State e Name
theApp =
    M.App { M.appDraw         = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent  = appEvent
          , M.appStartEvent   = return
          , M.appAttrMap      = const theMap
          }

-- Main function
main :: IO ()
main = do
  -- Arguments initialization
  args <- getArgs
  if not (checkArgs args) || "-h" `elem` args
    then usage
    else do
      let nbTrials = fromMaybe 10 $ getNbTrialsFromArgs args
      -- Game configuration
      let nbLetters = 4
      -- Secret word to guess
      gen <- getStdGen
      -- Init the game and start it
      let defaultEditor = E.editor Prompt (Just 1) ""
          game' = draw (createGame nbTrials ['0'..'9']) 4 gen
      -- print $ game' ^. getSecret
      -- c <- getChar
      state' <- M.defaultMain theApp (State game' defaultEditor Nothing)
      print $ show (D.dialogSelection $ fromJust $ state' ^. yesNo)