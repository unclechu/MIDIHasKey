{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module GUI
     ( runGUI
     , GUIContext (..)
     , GUIInterface (..)
     , KeyButtonStateUpdater
     ) where

import Prelude hiding (lookup)
import Prelude.Unicode
import GHC.TypeLits

import Data.Bool
import Data.Proxy
import Data.Maybe
import Data.HashMap.Strict

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Control.Concurrent.MVar

import Graphics.UI.Gtk
import Sound.MIDI.Message.Channel

-- local
import Types
import Utils
import Keys.Types
import Keys.Specific.GUI


data GUIContext
  = GUIContext
  { getPitchMapping    ∷ IO (HashMap RowKey Pitch)
  , appExitHandler     ∷ IO ()
  , panicButtonHandler ∷ IO ()
  , noteButtonHandler  ∷ RowKey → Bool → IO ()
  }

data GUIInterface
  = GUIInterface
  { keyButtonStateUpdate ∷ KeyButtonStateUpdater
  }

type KeyButtonStateUpdater = RowKey → Bool → IO ()


mainAppWindow ∷ GUIContext → MVar (RowKey, Bool) → IO ()
mainAppWindow ctx keyBtnStateBus = do
  wnd ← windowNew
  on wnd objectDestroy mainQuit

  set wnd [ containerBorderWidth := 8
          , windowTitle := symbolVal (Proxy ∷ Proxy WindowTitle)
          , windowModal := True
          ]

  pitchMap ← getPitchMapping ctx

  (allButtonsRows ∷ [[(RowKey, Button)]]) ←
    let getButton ∷ GUIKeyOfRow → IO (RowKey, Button)
        getButton (rowKey, label) = do btn ← buttonNew
                                       set btn [buttonLabel := btnLabel]
                                       on btn buttonPressEvent   $ tryEvent $ liftIO onPress
                                       on btn buttonReleaseEvent $ tryEvent $ liftIO onRelease
                                       pure (rowKey, btn)

          where onPress   = noteButtonHandler ctx rowKey True
                onRelease = noteButtonHandler ctx rowKey False

                btnLabel = case lookup rowKey pitchMap of
                                -- +1 to shift from [0..127] to [1..128]
                                Just x  → label ⧺ fmap superscript (show $ fromPitch x + 1)
                                Nothing → label

     in forM allGUIRows $ mapM getButton

  exitBtn ← buttonNew
  set exitBtn [buttonLabel := "Exit"]
  on exitBtn buttonActivated $ appExitHandler ctx

  panicBtn ← buttonNew
  set panicBtn [buttonLabel := "Panic"]
  on panicBtn buttonActivated $ panicButtonHandler ctx

  topButtons ← hBoxNew False 5
  containerAdd topButtons panicBtn
  containerAdd topButtons exitBtn

  keyRowsBox ← vBoxNew False 5

  set keyRowsBox [ widgetMarginLeft   := 8
                 , widgetMarginRight  := 8
                 , widgetMarginTop    := 5
                 , widgetMarginBottom := 8
                 ]

  mapM_ (containerAdd keyRowsBox) =<<
    forM (fmap snd <$> reverse allButtonsRows)
         (\keysButtons → do c ← hBoxNew False 5 ; c <$ mapM_ (containerAdd c) keysButtons)

  keyboardFrame ← frameNew
  set keyboardFrame [frameLabel := "Keyboard"]
  containerAdd keyboardFrame keyRowsBox

  mainBox ← vBoxNew False 5
  containerAdd mainBox topButtons
  containerAdd mainBox keyboardFrame

  containerAdd wnd mainBox
  widgetShowAll wnd

  let buttonsMap ∷ HashMap RowKey Button
      buttonsMap = unions $ fromList <$> allButtonsRows

  -- TODO FIXME it doesn't visually indicate anything
  void $ forkIO $ catchThreadFail "GUI listener for key button state updates" $ forever $ do
    (rowKey, bool buttonReleased buttonPressed → action) ← takeMVar keyBtnStateBus
    fromMaybe (pure ()) $ rowKey `lookup` buttonsMap <&> postGUIAsync ∘ action

myGUI ∷ GUIContext → MVar (RowKey, Bool) → IO ()
myGUI ctx keyBtnStateBus = do
  initGUI
  mainAppWindow ctx keyBtnStateBus
  mainGUI
  appExitHandler ctx

runGUI ∷ GUIContext → IO GUIInterface
runGUI ctx = do
  (keyBtnStateBus ∷ MVar (RowKey, Bool)) ← newEmptyMVar
  void $ forkIO $ catchThreadFail "Main GUI" $ myGUI ctx keyBtnStateBus
  pure GUIInterface { keyButtonStateUpdate = curry $ putMVar keyBtnStateBus }
