{-# LANGUAGE TemplateHaskell #-}
module Ui
( theApp
, GameState 
, createGameState
, getAnotherGame
)
where

import           Control.Monad

import           Data.Ratio

import qualified Brick.AttrMap                 as A
import qualified Brick.Main                    as M
import qualified Brick.Types                   as T
import           Brick.Util (on, bg)
import           Brick.Widgets.Core
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Dialog          as D
import qualified Brick.Widgets.Edit            as E

import           Data.List
import           Data.Maybe
import qualified Data.Text.Zipper              as Z

import qualified Graphics.Vty                  as V

import           Lens.Micro
import           Lens.Micro.TH

import           Game
  
data Name = Prompt deriving (Ord, Show, Eq)

data YesNo = Yes | No deriving (Show, Eq)

data Widget = Editor (E.Editor String Name) | Dialog (D.Dialog YesNo)

data GameState = GameState
                   { _game   :: Game
                   , _editor :: E.Editor String Name
                   , _yesNo  :: Maybe (D.Dialog YesNo)
                   }
makeLenses ''GameState

createGameState :: Game -> GameState
createGameState game = GameState game editor Nothing
  where editor = E.editor Prompt (Just 1) ""

getAnotherGame :: GameState -> Maybe Bool
getAnotherGame GameState { _yesNo = Nothing } = Nothing
getAnotherGame GameState { _game = game, _yesNo = (Just yn) } = do 
  guard $ status game /= Continue
  button <- D.dialogSelection yn
  return $ button == Yes

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
drawDialog :: GameState -> T.Widget Name
drawDialog GameState { _yesNo = Just yn, _game = game } =
  case endMsg $ status game of
    Nothing      -> emptyWidget
    Just endStr  -> translateBy (T.Location (0, offset)) 
                    $ D.renderDialog yn $ C.hCenter $ padAll 1 
                    $ str $ endStr ++ " Another game ?"
    where offset = ceiling (game ^. getNbTrials % 2) + 5
drawDialog _ = emptyWidget

-- Draw the main widget
drawMain :: GameState -> T.Widget Name
drawMain GameState { _game = g, _editor = e } = 
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
    guesses    = reverse $ take nbTrials $ g ^. getGuesses ++ repeat ""
    fstCol     = map (intersperse ' ') guesses
    sndCol     = map (showHint g) guesses
    re         = E.renderEditor (str . unlines) True e

-- Rendering function
drawUI :: GameState -> [T.Widget Name]
drawUI state = [ f state | f <- [drawDialog, drawMain] ]

-- Handle the dialog events
handleDialogEvent :: GameState -> T.BrickEvent Name e -> T.EventM Name (T.Next GameState)
handleDialogEvent state@GameState { _yesNo = (Just yn) } (T.VtyEvent ev) = do
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
showYesNoIfNeeded :: GameState -> Maybe (D.Dialog YesNo)
showYesNoIfNeeded state | status (state ^. game) == Continue = Nothing
                        | otherwise                          = Just yesNoDialog

-- Handle the editor events
handleEditorEvent :: GameState -> T.BrickEvent Name e -> T.EventM Name (T.Next GameState)
handleEditorEvent state@GameState { _editor = e, _game = g } (T.VtyEvent ev) = do
  -- The new editor with event handled
  editor' <- E.handleEditorEvent ev e
  -- Its content
  let guess = unwords $ E.getEditContents editor'
  -- We check the validity of the typed guess
  if not $ validPartialGuess g guess
    then M.continue state
    else
      let state' = if not $ validGuess g guess
                   -- The guess is not long enough, carry on
                   then state & editor .~ editor'
                   -- The guess is ok, append it to the guess list
                   -- and reset the editor
                   else state & editor %~ E.applyEdit Z.clearZipper 
                              & game   %~ addGuess guess
      -- If the guess is right or the number of trials is max then we need to 
      -- show the end dialog
      in  M.continue $ state' & yesNo .~ showYesNoIfNeeded state'
handleEditorEvent state _ = M.continue state

no :: D.Dialog YesNo -> D.Dialog YesNo
no d = d & D.dialogSelectedIndexL ?~ 1

-- Is the dialog visible ie does it exists
dialogVisible :: GameState -> Bool
dialogVisible s = isJust (s ^. yesNo)

-- Return the current widget
frontWidget :: GameState -> Widget
frontWidget GameState { _yesNo = Just yn } = Dialog yn
frontWidget GameState { _editor = e }      = Editor e

-- Event dispatcher
handleEvent :: Widget -> GameState -> T.BrickEvent Name e -> T.EventM Name (T.Next GameState)
handleEvent (Dialog d) = handleDialogEvent
handleEvent (Editor e) = handleEditorEvent

-- Event handler
appEvent :: GameState -> T.BrickEvent Name e -> T.EventM Name (T.Next GameState)
-- Quit the game
appEvent state (T.VtyEvent (V.EvKey V.KEsc [])) = 
  M.halt $ state & yesNo ?~ no yesNoDialog
-- Main event handler
appEvent state ev = handleEvent (frontWidget state) state ev

-- Selected button black
theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.buttonSelectedAttr, bg V.black)
    ]

-- Main record for the brick application
theApp :: M.App GameState e Name
theApp =
    M.App { M.appDraw         = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent  = appEvent
          , M.appStartEvent   = return
          , M.appAttrMap      = const theMap
          }
