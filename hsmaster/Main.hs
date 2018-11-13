{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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

import qualified Game                          as G

data Name = Prompt
          deriving (Ord, Show, Eq)

type Game = G.Game (E.Editor String Name)

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
  where (f, s) = G.compute game guess

-- Rendering function
drawUI :: Game -> [T.Widget Name]
drawUI game = [ui]
 where
  e        = E.renderEditor (str . unlines) True (G.editor game)
  guesses' = reverse $ take (G.nbTrials game) $ G.guesses game ++ repeat ""
  fstCol   = map (intersperse ' ') guesses'
  hint     = map (showHint game) guesses'
  ui       = C.center $ hLimit 25 $ B.border $ vBox
    [ hBox
      [ padLeftRight 2 $ C.hCenter $ str $ unlines fstCol
      , vLimit (G.nbTrials game) B.vBorder
      , padLeftRight 2 $ str $ unlines hint
      ]
    , B.hBorder
    , str "> " <+> e
    ]

-- End of game message
endMsg :: G.Status -> Maybe String
endMsg G.Won  = Just "You won !"
endMsg G.Lost = Just "You loose !"
endMsg _    = Nothing

-- Edit a zipper to write a message
showMsg :: Monoid a => String -> Z.TextZipper a -> Z.TextZipper a
showMsg msg = foldl (>>>) Z.clearZipper $ map Z.insertChar msg

-- Event handler
appEvent :: Game -> T.BrickEvent Name e -> T.EventM Name (T.Next Game)
-- Quit the game
appEvent game (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt game
-- Cheat mode, press h to show the secret number
appEvent game (T.VtyEvent (V.EvKey (V.KChar 'h') [])) =
  M.continue $ G.applyEditor game $ E.applyEdit (showMsg (G.secret game)) 
-- Main event handler
appEvent game (T.VtyEvent ev) = if G.status game /= G.Continue
  then M.halt game
  else do
    -- The new editor with event handled
    editor' <- E.handleEditorEvent ev (G.editor game)
    -- Its content
    let guess = unwords $ E.getEditContents editor'
    -- We check the validity of the typed guess
    if not $ G.validPartialGuess game guess
      then M.continue game
      else
        -- Its length is still wrong
        if not $ G.validGuess game guess
        then M.continue $ G.setEditor game editor'
        else do
          -- Append guess to stack of guesses
          let game' = G.addGuess game guess
          -- Show the message if needed
          let zipper = case endMsg (G.status game') of
                Nothing  -> Z.clearZipper
                Just msg -> showMsg msg
          -- Carry on
          M.continue $ G.applyEditor game' $ E.applyEdit zipper 

appEvent game _ = M.continue game

-- Main record for the brick application
theApp :: M.App Game e Name
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
          game = G.draw (G.createGame nbTrials ['0'..'9'] defaultEditor) 4 gen
      void $ M.defaultMain theApp game
