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
import qualified Text.Read                     as TR

import           Game

data Name = Prompt
          deriving (Ord, Show, Eq)

data State = State { _game   :: Game
                   , _editor :: E.Editor String Name
                   , _hint   :: Bool}
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

-- Show a message in a separate layer
msgWidget :: State -> String -> T.Widget n
msgWidget state msg = let offset = T.Location (0, state ^. game . getNbTrials + 5)
  in C.centerLayer $ translateBy offset
    $ B.border $ padLeftRight 3 $ padTopBottom 1 $ str msg

-- Create a simple widget showing the eog message
endMsgWidget :: State -> Maybe (T.Widget n)
endMsgWidget state = do
  msg <- endMsg $ status (state ^. game)
  return $ msgWidget state msg

-- Rendering function
drawUI :: State -> [T.Widget Name]
drawUI state = msg:[mainWidget]
  where
    msg        = if state ^. hint 
      then msgWidget state $ state ^. game . getSecret
      else fromMaybe emptyWidget $ endMsgWidget state
    g          = state ^. game
    e          = E.renderEditor (str . unlines) True $ state ^. editor
    guesses    = reverse $ take (g ^. getNbTrials) $ g ^. getGuesses ++ repeat ""
    fstCol     = map (intersperse ' ') guesses
    sndCol     = map (showHint g) guesses
    mainWidget = C.center $ hLimit 25 $ B.border $ vBox
      [ hBox
        [ padLeftRight 2 $ C.hCenter $ str $ unlines fstCol
        , vLimit (g ^. getNbTrials) B.vBorder
        , padLeftRight 2 $ str $ unlines sndCol
        ]
      , B.hBorder
      , str "> " <+> e
      ]

-- Event handler
appEvent :: State -> T.BrickEvent Name e -> T.EventM Name (T.Next State)
-- Quit the game
appEvent state (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt state
-- Cheat mode, press h to show the secret number
appEvent state (T.VtyEvent (V.EvKey (V.KChar 'h') [])) =
  M.continue $ state & hint .~ True
-- Main event handler
appEvent state (T.VtyEvent ev)
  -- Show the hint
  | state ^. hint = M.continue $ state & hint .~ False
  -- End of the game
  | status (state ^. game) /= Continue = M.halt state
  -- Main
  | otherwise = do
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
            else state & game   %~ addGuess guess
                       & editor %~ E.applyEdit Z.clearZipper
      in M.continue state'
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
      let nbTrials  = fromMaybe 10 $ getNbTrialsFromArgs args
      -- Game configuration
      let nbLetters = 4
      -- Secret word to guess
      gen <- getStdGen
      -- Init the game and start it
      let defaultEditor = E.editor Prompt (Just 1) ""
          game' = draw (createGame nbTrials ['0'..'9']) 4 gen
          --game' = game_ & getNbTrials .~ 2
      void $ M.defaultMain theApp (State game' defaultEditor False)
