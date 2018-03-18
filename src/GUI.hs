{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module GUI
     ( runGUI
     , GUIContext (..)
     , GUIInitialValues (..)
     , GUIInterface (..)
     , GUIStateUpdate (..)
     ) where

import Prelude hiding (lookup)
import Prelude.Unicode
import GHC.TypeLits

import Data.Proxy
import Data.Maybe
import Data.HashMap.Strict
import Text.InterpolatedString.QM

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
  { initialValues        ∷ GUIInitialValues
  , appExitHandler       ∷ IO ()
  , panicButtonHandler   ∷ IO ()
  , selectChannelHandler ∷ Channel → IO ()
  , noteButtonHandler    ∷ RowKey → 𝔹 → IO ()
  }

data GUIInitialValues
  = GUIInitialValues
  { initialBaseKey        ∷ RowKey
  , initialBasePitch      ∷ Pitch
  , initialPitchMapping   ∷ HashMap RowKey Pitch
  , initialChannel        ∷ Channel
  , initialVelocity       ∷ Velocity
  , initialOctave         ∷ Octave
  , initialNotesPerOctave ∷ NotesPerOctave
  }

data GUIInterface
  = GUIInterface
  { guiStateUpdate ∷ GUIStateUpdate → IO ()
  }

data GUIStateUpdate
  = ChannelChange  Channel
  | KeyButtonState RowKey 𝔹
  deriving (Show, Eq)


mainAppWindow ∷ GUIContext → CssProvider → MVar GUIStateUpdate → IO ()
mainAppWindow ctx cssProvider stateUpdateBus = do
  wnd ← windowNew
  on wnd objectDestroy mainQuit

  set wnd [ containerBorderWidth := 8
          , windowTitle := symbolVal (Proxy ∷ Proxy WindowTitle)
          , windowModal := True
          ]

  let pitchMapping   = initialPitchMapping   $ initialValues ctx
      currentChannel = initialChannel        $ initialValues ctx
      notesPerOctave = initialNotesPerOctave $ initialValues ctx

  (allButtonsRows ∷ [[(RowKey, Button)]]) ←
    let colorsCount = 8
        perOctave   = fromIntegral $ fromNotesPerOctave notesPerOctave

        getButton ∷ GUIKeyOfRow → IO (RowKey, Button)
        getButton (rowKey, label) = do
          btn ← buttonNew
          set btn [buttonLabel := btnLabel]
          on btn buttonPressEvent   $ tryEvent $ liftIO onPress
          on btn buttonReleaseEvent $ tryEvent $ liftIO onRelease

          case btnClass of
               Just x  → void $ withCssClass cssProvider x btn
               Nothing → pure ()

          pure (rowKey, btn)

          where
            onPress    = noteButtonHandler ctx rowKey True
            onRelease  = noteButtonHandler ctx rowKey False
            basePitch  = fromPitch $ initialBasePitch $ initialValues ctx
            foundPitch = lookup rowKey pitchMapping <&> fromPitch

            btnLabel = case foundPitch of
                            -- +1 to shift from [0..127] to [1..128]
                            Just x  → label ⧺ fmap superscript (show $ succ x)
                            Nothing → label

            btnClass :: Maybe String
            btnClass = do
              x ← foundPitch <&> subtract basePitch <&> fromIntegral

              pure $
                if x ≥ 0
                   then let n = floor $ x / perOctave
                         in [qm| btn-octave-{succ $ n `mod` colorsCount} |]

                   else let n = floor $ (negate x - 1) / perOctave
                         in [qm| btn-octave-{succ $ pred colorsCount - (n `mod` colorsCount)} |]

     in forM allGUIRows $ mapM getButton

  exitBtn ← buttonNew
  set exitBtn [buttonLabel := "Exit"]
  on exitBtn buttonActivated $ appExitHandler ctx

  panicBtn ← buttonNew
  set panicBtn [buttonLabel := "Panic"]
  on panicBtn buttonActivated $ panicButtonHandler ctx

  menu ← menuNew
  set menu [menuTitle := "Select a MIDI channel"]

  forM_ [(minBound :: Channel) .. maxBound] $ \ch → do
    menuItem ← menuItemNew
    set menuItem [menuItemLabel := show $ succ $ fromChannel ch]
    on menuItem menuItemActivated $ selectChannelHandler ctx ch
    menuShellAppend menu menuItem

  widgetShowAll menu

  channelBtn ← buttonNew
  let getChannelBtnLabel ch = [qm| Channel: {succ $ fromChannel ch} |] :: String
  set channelBtn [buttonLabel := getChannelBtnLabel currentChannel]
  on channelBtn buttonActivated $ menuPopup menu Nothing

  topButtons ← hBoxNew False 5
  containerAdd topButtons panicBtn
  containerAdd topButtons channelBtn
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

  void $ forkIO $ catchThreadFail "GUI listener for GUI state updates" $ forever $
    takeMVar stateUpdateBus >>= \case

      ChannelChange ch →
        postGUIAsync $ void $ set channelBtn [buttonLabel := getChannelBtnLabel ch]

      KeyButtonState rowKey isPressed →
        fromMaybe (pure ()) $ rowKey `lookup` buttonsMap <&> \w → do
          styleContext ← widgetGetStyleContext w
          postGUIAsync $ let f = if isPressed then styleContextAddClass else styleContextRemoveClass
                          in f styleContext "active"


myGUI ∷ GUIContext → MVar GUIStateUpdate → IO ()
myGUI ctx stateUpdateBus = do
  initGUI
  cssProvider ← getCssProvider
  mainAppWindow ctx cssProvider stateUpdateBus
  mainGUI
  appExitHandler ctx

runGUI ∷ GUIContext → IO GUIInterface
runGUI ctx = do
  (stateUpdateBus ∷ MVar GUIStateUpdate) ← newEmptyMVar
  void $ forkIO $ catchThreadFail "Main GUI" $ myGUI ctx stateUpdateBus
  pure GUIInterface { guiStateUpdate = putMVar stateUpdateBus }


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
