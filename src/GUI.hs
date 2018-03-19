{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module GUI
     ( runGUI
     , GUIContext (..)
     , GUIState (..)
     , GUIInterface (..)
     , GUIStateUpdate (..)
     ) where

import Prelude hiding (lookup)
import Prelude.Unicode
import GHC.TypeLits
import Foreign.C.Types

import Data.IORef
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
  { initialValues        ∷ GUIState
  , appExitHandler       ∷ IO ()
  , panicButtonHandler   ∷ IO ()
  , setBaseKeyHandler    ∷ RowKey → IO ()
  , setBasePitchHandler  ∷ Pitch → IO ()
  , selectChannelHandler ∷ Channel → IO ()
  , noteButtonHandler    ∷ RowKey → 𝔹 → IO ()
  }

data GUIState
  = GUIState
  { guiStateBaseKey        ∷ RowKey
  , guiStateBasePitch      ∷ Pitch
  , guiStatePitchMapping   ∷ HashMap RowKey Pitch
  , guiStateChannel        ∷ Channel
  , guiStateVelocity       ∷ Velocity
  , guiStateOctave         ∷ Octave
  , guiStateNotesPerOctave ∷ NotesPerOctave
  }

data GUIInterface
  = GUIInterface
  { guiStateUpdate ∷ GUIStateUpdate → IO ()
  }

data GUIStateUpdate
  = SetBaseKey        RowKey
  | SetBasePitch      Pitch
  | SetPitchMapping   (HashMap RowKey Pitch)
  | SetChannel        Channel
  | SetVelocity       Velocity
  | SetOctave         Octave
  | SetNotesPerOctave NotesPerOctave
  | KeyButtonState    RowKey 𝔹
  deriving (Show, Eq)


mainAppWindow ∷ GUIContext → CssProvider → MVar GUIStateUpdate → IO ()
mainAppWindow ctx cssProvider stateUpdateBus = do
  guiStateRef ← newIORef $ initialValues ctx

  wnd ← do
    wnd ← windowNew
    on wnd objectDestroy mainQuit

    set wnd [ containerBorderWidth := 8
            , windowTitle := symbolVal (Proxy ∷ Proxy WindowTitle)
            , windowModal := True
            ]

    pure wnd

  let allGUIKeys  = mconcat allGUIRows
      keyLabelMap = fromList allGUIKeys
      colorsCount = 8

      getButtonLabelAndClass
        ∷ Pitch → HashMap RowKey Pitch → NotesPerOctave → RowKey → String → (String, Maybe String)

      getButtonLabelAndClass basePitch pitchMapping perOctave rowKey keyLabel = (label, className)
        where
          foundPitch = lookup rowKey pitchMapping <&> fromPitch

          label = case foundPitch of
                       -- +1 to shift from [0..127] to [1..128]
                       Just x  → [qm| <b>{keyLabel}</b> <i><small>{succ x}</small></i> |]
                       Nothing → [qm| <b>{keyLabel}</b> |] ∷ String

          className ∷ Maybe String
          className = do
            x ← foundPitch <&> subtract (fromPitch basePitch) <&> fromIntegral

            pure $
              if x ≥ 0
                 then let n = floor $ x / fromIntegral (fromNotesPerOctave perOctave)
                       in [qm| btn-octave-{succ $ n `mod` colorsCount} |]

                 else let n = floor $ (negate x - 1) / fromIntegral (fromNotesPerOctave perOctave)
                       in [qm| btn-octave-{succ $ pred colorsCount - (n `mod` colorsCount)} |]

  (allButtonsRows, allButtons) ← do
    let getButton ∷ GUIKeyOfRow → IO (RowKey, (Button, String → IO ()))
        getButton (rowKey, keyLabel) = do
          label ← labelNew (Nothing ∷ Maybe String)
          labelSetMarkup label btnLabel

          btn ← buttonNew
          containerAdd btn label
          on btn buttonPressEvent   $ tryEvent $ liftIO onPress
          on btn buttonReleaseEvent $ tryEvent $ liftIO onRelease
          btnClass `maybeMUnit'` \className → withCssClass cssProvider className btn

          pure (rowKey, (btn, labelSetMarkup label ∷ String → IO ()))

          where
            onPress   = noteButtonHandler ctx rowKey True
            onRelease = noteButtonHandler ctx rowKey False

            (btnLabel, btnClass) =
              let v = initialValues ctx in
              getButtonLabelAndClass (guiStateBasePitch v)
                                     (guiStatePitchMapping v)
                                     (guiStateNotesPerOctave v)
                                     rowKey keyLabel

    (rows ∷ [[(RowKey, (Button, String → IO ()))]]) ← forM allGUIRows $ mapM getButton
    pure (rows, mconcat rows)

  exitEl ← do
    btn ← buttonNew
    set btn [buttonLabel := "Exit"]
    on btn buttonActivated $ appExitHandler ctx
    pure btn

  panicEl ← do
    btn ← buttonNew
    set btn [buttonLabel := "Panic"]
    on btn buttonActivated $ panicButtonHandler ctx
    pure btn

  (channelEl, channelUpdater) ← do
    menu ← do
      menu ← menuNew
      set menu [menuTitle := "Select MIDI channel"]

      forM_ [(minBound :: Channel) .. maxBound] $ \ch → do
        menuItem ← menuItemNew
        set menuItem [menuItemLabel := show $ succ $ fromChannel ch]
        on menuItem menuItemActivated $ selectChannelHandler ctx ch
        menuShellAppend menu menuItem

      menu <$ widgetShowAll menu

    label ← labelNew (Nothing ∷ Maybe String)
    let getLabel ch = [qm| Channel: <b>{succ $ fromChannel ch}</b> |] :: String
    labelSetMarkup label $ getLabel $ guiStateChannel $ initialValues ctx

    btn ← buttonNew
    containerAdd btn label
    on btn buttonActivated $ menuPopup menu Nothing
    pure (btn, getLabel • labelSetMarkup label)

  (baseKeyEl, baseKeyUpdater) ← do
    menu ← do
      menu ← menuNew
      set menu [menuTitle := "Select base key"]

      forM_ allGUIKeys $ \(rowKey, keyLabel) → do
        menuItem ← menuItemNew
        set menuItem [menuItemLabel := keyLabel]
        on menuItem menuItemActivated $ setBaseKeyHandler ctx rowKey
        menuShellAppend menu menuItem

      menu <$ widgetShowAll menu

    label ← labelNew (Nothing ∷ Maybe String)
    let getLabel rowKey = [qm| Base key: <b>{keyLabelMap ! rowKey}</b> |] :: String
    labelSetMarkup label $ getLabel $ guiStateBaseKey $ initialValues ctx

    btn ← buttonNew
    containerAdd btn label
    on btn buttonActivated $ menuPopup menu Nothing
    pure (btn, getLabel • labelSetMarkup label)

  (basePitchEl, basePitchUpdater) ← do
    let val = fromIntegral $ succ $ fromPitch $ guiStateBasePitch $ initialValues ctx
        minPitch = succ $ fromIntegral $ fromPitch minBound
        maxPitch = succ $ fromIntegral $ fromPitch maxBound

    btn ← spinButtonNewWithRange minPitch maxPitch 1
    set btn [spinButtonValue := val]

    label ← labelNew $ Just "Base pitch:"

    box ← vBoxNew False 5
    containerAdd box label
    containerAdd box btn

    connectGeneric "value-changed" True btn $ \_ → do
      x ← spinButtonGetValueAsInt btn
      setBasePitchHandler ctx $ toPitch $ pred x
      pure (0 ∷ CInt)

    pure (box, spinButtonSetValue btn ∘ fromIntegral ∘ succ ∘ fromPitch)

  topButtons ← do
    box ← hBoxNew False 5
    containerAdd box panicEl
    containerAdd box channelEl
    containerAdd box baseKeyEl
    containerAdd box basePitchEl
    containerAdd box exitEl
    pure box

  keyRowsBox ← do
    box ← vBoxNew False 5

    set box [ widgetMarginLeft   := 8
            , widgetMarginRight  := 8
            , widgetMarginTop    := 5
            , widgetMarginBottom := 8
            ]

    mapM_ (containerAdd box) =<<
      forM (fmap (snd • fst) <$> reverse allButtonsRows)
           (\keysButtons → do c ← hBoxNew False 5 ; c <$ mapM_ (containerAdd c) keysButtons)

    pure box

  keyboardFrame ← do
    frame ← frameNew
    set frame [frameLabel := "Keyboard"]
    containerAdd frame keyRowsBox
    pure frame

  mainBox ← do
    box ← vBoxNew False 5
    containerAdd box topButtons
    containerAdd box keyboardFrame
    pure box

  containerAdd wnd mainBox
  widgetShowAll wnd

  let buttonsMap ∷ HashMap RowKey (Button, String → IO ())
      buttonsMap = fromList allButtons

      updateButton
        ∷ Pitch → HashMap RowKey Pitch → NotesPerOctave
        → (RowKey, (Button, String → IO ())) → IO ()

      updateButton basePitch pitchMapping perOctave (rowKey, (btn, labelUpdater)) = do
        let keyLabel = keyLabelMap ! rowKey

            (btnLabel, className) =
              getButtonLabelAndClass basePitch pitchMapping perOctave rowKey keyLabel

        styleContext ← widgetGetStyleContext btn
        forM_ colors $ removeColorClass styleContext
        styleContextAddClass styleContext `maybeMUnit` className
        labelUpdater btnLabel

        where
          colors = [1..colorsCount]
          removeColorClass c n = styleContextRemoveClass c ([qm| btn-octave-{n} |] ∷ String)

      updateButtons ∷ IO ()
      updateButtons = do
        s ← readIORef guiStateRef

        forM_ allButtons $
          updateButton (guiStateBasePitch s) (guiStatePitchMapping s) (guiStateNotesPerOctave s)

  void $ forkIO $ catchThreadFail "GUI listener for GUI state updates" $ forever $
    takeMVar stateUpdateBus >>= \case
      SetBaseKey k → do
        modifyIORef guiStateRef $ \s → s { guiStateBaseKey = k }
        postGUIAsync $ baseKeyUpdater k >> updateButtons

      SetBasePitch p → do
        modifyIORef guiStateRef $ \s → s { guiStateBasePitch = p }
        postGUIAsync $ basePitchUpdater p >> updateButtons

      SetPitchMapping mapping → do
        modifyIORef guiStateRef $ \s → s { guiStatePitchMapping = mapping }
        postGUIAsync updateButtons

      SetChannel ch → do
        modifyIORef guiStateRef $ \s → s { guiStateChannel = ch }
        postGUIAsync $ channelUpdater ch

      SetVelocity vel →
        modifyIORef guiStateRef $ \s → s { guiStateVelocity = vel }

      SetOctave octave → do
        modifyIORef guiStateRef $ \s → s { guiStateOctave = octave }
        postGUIAsync updateButtons

      SetNotesPerOctave perOctave → do
        modifyIORef guiStateRef $ \s → s { guiStateNotesPerOctave = perOctave }
        postGUIAsync updateButtons

      KeyButtonState rowKey isPressed →
        fromMaybe (pure ()) $ rowKey `lookup` buttonsMap <&> \(w, _) → postGUIAsync $ do
          styleContext ← widgetGetStyleContext w
          let f = if isPressed then styleContextAddClass else styleContextRemoveClass
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
