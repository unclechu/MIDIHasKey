{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GUI
     ( runGUI
     , GUIContext (..)
     , GUIInitialValues (..)
     , GUIInterface (..)
     , KeyButtonStateUpdater
     ) where

import Prelude hiding (lookup)
import Prelude.Unicode
import GHC.TypeLits

import Data.Proxy
import Data.Maybe
import Data.HashMap.Strict

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Control.Concurrent.MVar

import System.Glib.UTFString
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.CssProvider
import Graphics.UI.Gtk.General.StyleContext
import Sound.MIDI.Message.Channel

-- local
import Types
import Utils
import Keys.Types
import Keys.Specific.GUI


data GUIContext
  = GUIContext
  { initialValues      ∷ GUIInitialValues
  , appExitHandler     ∷ IO ()
  , panicButtonHandler ∷ IO ()
  , noteButtonHandler  ∷ RowKey → Bool → IO ()
  }

data GUIInitialValues
  = GUIInitialValues
  { initialPitchMapping   ∷ HashMap RowKey Pitch
  , initialChannel        ∷ Channel
  , initialVelocity       ∷ Velocity
  , initialOctave         ∷ Octave
  , initialNotesPerOctave ∷ NotesPerOctave
  }

data GUIInterface
  = GUIInterface
  { keyButtonStateUpdate ∷ KeyButtonStateUpdater
  }

type KeyButtonStateUpdater = RowKey → Bool → IO ()


mainAppWindow ∷ GUIContext → CssProvider → MVar (RowKey, Bool) → IO ()
mainAppWindow ctx cssProvider keyBtnStateBus = do
  wnd ← windowNew
  on wnd objectDestroy mainQuit

  set wnd [ containerBorderWidth := 8
          , windowTitle := symbolVal (Proxy ∷ Proxy WindowTitle)
          , windowModal := True
          ]

  let pitchMapping = initialPitchMapping $ initialValues ctx
      currentChannel = initialChannel $ initialValues ctx

  (allButtonsRows ∷ [[(RowKey, Button)]]) ←
    let getButton ∷ GUIKeyOfRow → IO (RowKey, Button)
        getButton (rowKey, label) = do btn ← buttonNew
                                       set btn [buttonLabel := btnLabel]
                                       on btn buttonPressEvent   $ tryEvent $ liftIO onPress
                                       on btn buttonReleaseEvent $ tryEvent $ liftIO onRelease
                                       pure (rowKey, btn)

          where onPress   = noteButtonHandler ctx rowKey True
                onRelease = noteButtonHandler ctx rowKey False

                btnLabel = case lookup rowKey pitchMapping of
                                -- +1 to shift from [0..127] to [1..128]
                                Just x  → label ⧺ fmap superscript (show $ fromPitch x + 1)
                                Nothing → label

     in forM allGUIRows $ mapM getButton

  exitBtn ← buttonNew
  set exitBtn [buttonLabel := "Exit"]
  on exitBtn buttonActivated $ appExitHandler ctx
  void $ withCssClass cssProvider "btn-danger" exitBtn

  panicBtn ← buttonNew
  set panicBtn [buttonLabel := "Panic"]
  on panicBtn buttonActivated $ panicButtonHandler ctx

  -- TODO FIXME
  menu ← menuNew
  set menu [menuTitle := "Select a MIDI channel"]

  -- TODO FIXME
  menuItem ← menuItemNew
  set menuItem [menuItemLabel := "some item"]
  on menuItem menuItemActivated $ putStrLn "some item is activated"

  -- TODO FIXME
  menuShellAppend menu menuItem

  -- TODO FIXME
  channelBtn ← buttonNew
  set channelBtn [buttonLabel := "Channel: " ⧺ show (succ $ fromChannel currentChannel)]
  on channelBtn buttonActivated $ menuPopup menu Nothing

  topButtons ← hBoxNew False 5
  containerAdd topButtons panicBtn
  -- containerAdd topButtons channelBtn -- TODO FIXME
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

  void $ forkIO $ catchThreadFail "GUI listener for key button state updates" $ forever $ do
    (rowKey, isPressed) ← takeMVar keyBtnStateBus

    when isPressed $ fromMaybe (pure ()) $
      rowKey `lookup` buttonsMap <&> postGUIAsync ∘ void ∘ widgetActivate


myGUI ∷ GUIContext → MVar (RowKey, Bool) → IO ()
myGUI ctx keyBtnStateBus = do
  initGUI
  cssProvider ← getCssProvider
  mainAppWindow ctx cssProvider keyBtnStateBus
  mainGUI
  appExitHandler ctx

runGUI ∷ GUIContext → IO GUIInterface
runGUI ctx = do
  (keyBtnStateBus ∷ MVar (RowKey, Bool)) ← newEmptyMVar
  void $ forkIO $ catchThreadFail "Main GUI" $ myGUI ctx keyBtnStateBus
  pure GUIInterface { keyButtonStateUpdate = curry $ putMVar keyBtnStateBus }


getCssProvider ∷ IO CssProvider
getCssProvider = do
  cssProvider ← cssProviderNew
  cssProvider <$ cssProviderLoadFromPath cssProvider "./gtk-custom.css"

-- Priority range is [1..800]. See also:
-- https://www.stackage.org/haddock/lts-9.21/gtk3-0.14.8/src/Graphics.UI.Gtk.General.StyleContext.html#styleContextAddProvider
maxCssPriority ∷ Int
maxCssPriority = 800

bindCssProvider ∷ WidgetClass widget ⇒ CssProvider → widget → IO StyleContext
bindCssProvider cssProvider w = do
  styleContext ← widgetGetStyleContext w
  styleContext <$ styleContextAddProvider styleContext cssProvider maxCssPriority

withCssClass ∷ (WidgetClass w, GlibString s) ⇒ CssProvider → s → w → IO StyleContext
withCssClass cssProvider className w = do
  styleContext ← bindCssProvider cssProvider w
  styleContext <$ styleContextAddClass styleContext className
