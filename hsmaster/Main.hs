{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Reader
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

import           Game

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
drawUI :: Game -> [T.Widget Name]
drawUI game = [ui]
 where
  e        = E.renderEditor (str . unlines) True (editor game)
  guesses' = reverse $ take (nbTrials game) $ guesses game ++ repeat ""
  fstCol   = map (intersperse ' ') guesses'
  hint     = map (showHint game) guesses'
  ui       = C.center $ hLimit 25 $ B.border $ vBox
    [ hBox
      [ padLeftRight 2 $ C.hCenter $ str $ unlines fstCol
      , vLimit (nbTrials game) B.vBorder
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
appEvent :: Game -> T.BrickEvent Name e -> T.EventM Name (T.Next Game)
-- Quit the game
appEvent game (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt game
-- Cheat mode, press h to show the secret number
appEvent game (T.VtyEvent (V.EvKey (V.KChar 'h') [])) =
  M.continue $ applyEditor game $ E.applyEdit (showMsg (secret game)) 
-- Main event handler
appEvent game (T.VtyEvent ev) = if status game /= Continue
  then M.halt game
  else do
    -- The new editor with event handled
    editor' <- E.handleEditorEvent ev (editor game)
    -- Its content
    let guess = unwords $ E.getEditContents editor'
    -- We check the validity of the typed guess
    if not $ validPartialGuess (length $ secret game) (values game) guess
      then M.continue game
      else
        -- Its length is still wrong
        if length guess < length (secret game)
        then M.continue $ setEditor game editor'
        else do
          -- Append guess to stack of guesses
          let game' = addGuess game guess
          -- Show the message if needed
          let zipper = case endMsg (status game') of
                Nothing  -> Z.clearZipper
                Just msg -> showMsg msg
          -- Carry on
          M.continue $ applyEditor game' $ E.applyEdit zipper 

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
      let game = draw (initGame nbTrials ['0'..'9']) 4 gen
      void $ M.defaultMain theApp game
