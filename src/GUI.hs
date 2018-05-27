{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitNamespaces #-}

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
import Data.Text (type Text)
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
  { initialValues            ∷ GUIState

  , appExitHandler           ∷ IO ()
  , panicButtonHandler       ∷ IO ()

  , setBaseKeyHandler        ∷ RowKey → IO ()
  , setBasePitchHandler      ∷ Pitch → IO ()
  , setOctaveHandler         ∷ Octave → IO ()
  , setBaseOctaveHandler     ∷ BaseOctave → IO ()
  , setNotesPerOctaveHandler ∷ NotesPerOctave → IO ()

  , selectChannelHandler     ∷ Channel → IO ()

  , noteButtonHandler        ∷ RowKey → 𝔹 → IO ()
  }

data GUIState
  = GUIState
  { guiStateBaseKey        ∷ RowKey
  , guiStateBasePitch      ∷ Pitch
  , guiStateOctave         ∷ Octave
  , guiStateBaseOctave     ∷ BaseOctave
  , guiStateNotesPerOctave ∷ NotesPerOctave

  , guiStatePitchMapping   ∷ HashMap RowKey Pitch

  , guiStateChannel        ∷ Channel
  , guiStateVelocity       ∷ Velocity
  }

data GUIInterface
  = GUIInterface
  { guiStateUpdate ∷ GUIStateUpdate → IO ()
  , guiShowAlert   ∷ AlertMessage → IO ()
  }

data GUIStateUpdate
  = SetBaseKey        RowKey
  | SetBasePitch      Pitch
  | SetOctave         Octave
  | SetBaseOctave     BaseOctave
  | SetNotesPerOctave NotesPerOctave

  | SetPitchMapping   (HashMap RowKey Pitch)

  | SetChannel        Channel
  | SetVelocity       Velocity

  | KeyButtonState    RowKey 𝔹
  deriving (Show, Eq)


mainAppWindow ∷ GUIContext → CssProvider → MVar GUIStateUpdate → IO Window
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
        ∷ Pitch → HashMap RowKey Pitch
        → Octave → NotesPerOctave
        → RowKey → String
        → (String, Maybe String)

      getButtonLabelAndClass basePitch pitchMapping octave perOctave rowKey keyLabel =
        (label, className)
        where
          foundPitch = lookup rowKey pitchMapping <&> fromPitch

          label = case foundPitch of
                       -- +1 to shift from [0..127] to [1..128]
                       Just x  → [qm| <b>{keyLabel}</b> <i><small>{succ x}</small></i> |]
                       Nothing → [qm| <b>{keyLabel}</b> |] ∷ String

          className ∷ Maybe String
          className = do
            x ← foundPitch <&> subtract (fromPitch basePitch) <&> fromIntegral

            let octaveN    = pred $ fromIntegral $ fromOctave octave     ∷ Double
                perOctaveN = fromIntegral $ fromNotesPerOctave perOctave ∷ Double

            pure $
              if x ≥ 0
                 then let n = floor $ x ÷ perOctaveN
                       in [qm| btn-octave-{succ $ n `mod` colorsCount} |]

                 else let n = floor $ (negate x - 1) ÷ perOctaveN
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
                                     (guiStateOctave v)
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

      forM_ [(minBound ∷ Channel) .. maxBound] $ \ch → do
        menuItem ← menuItemNew
        set menuItem [menuItemLabel := show $ succ $ fromChannel ch]
        on menuItem menuItemActivated $ selectChannelHandler ctx ch
        menuShellAppend menu menuItem

      menu <$ widgetShowAll menu

    label ← labelNew (Nothing ∷ Maybe String)
    let getLabel ch = [qm| Channel: <b>{succ $ fromChannel ch}</b> |] ∷ String
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
    let getLabel rowKey = [qm| Base key: <b>{keyLabelMap ! rowKey}</b> |] ∷ String
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

  (octaveEl, octaveUpdater) ← do
    let val = fromIntegral $ fromOctave $ guiStateOctave $ initialValues ctx
        minOctave = fromIntegral $ fromOctave minBound
        maxOctave = fromIntegral $ fromOctave maxBound

    btn ← spinButtonNewWithRange minOctave maxOctave 1
    set btn [spinButtonValue := val]

    label ← labelNew $ Just "Octave:"

    box ← vBoxNew False 5
    containerAdd box label
    containerAdd box btn

    connectGeneric "value-changed" True btn $ \_ → do
      x ← fromIntegral <$> spinButtonGetValueAsInt btn
      setOctaveHandler ctx $ toOctave x
      pure (0 ∷ CInt)

    pure (box, spinButtonSetValue btn ∘ fromIntegral ∘ fromOctave)

  (baseOctaveEl, baseOctaveUpdater) ← do
    let val = fromIntegral $ fromBaseOctave' $ guiStateBaseOctave $ initialValues ctx
        minOctave = fromIntegral $ fromOctave minBound
        maxOctave = fromIntegral $ fromOctave maxBound

    btn ← spinButtonNewWithRange minOctave maxOctave 1
    set btn [spinButtonValue := val]

    label ← labelNew $ Just "Base octave:"

    box ← vBoxNew False 5
    containerAdd box label
    containerAdd box btn

    connectGeneric "value-changed" True btn $ \_ → do
      x ← fromIntegral <$> spinButtonGetValueAsInt btn
      setBaseOctaveHandler ctx $ toBaseOctave' x
      pure (0 ∷ CInt)

    pure (box, spinButtonSetValue btn ∘ fromIntegral ∘ fromBaseOctave')

  (notesPerOctaveEl, notesPerOctaveUpdater) ← do
    let val = fromIntegral $ fromNotesPerOctave $ guiStateNotesPerOctave $ initialValues ctx
        minV = fromIntegral $ fromNotesPerOctave minBound
        maxV = fromIntegral $ fromNotesPerOctave maxBound

    btn ← spinButtonNewWithRange minV maxV 1
    set btn [spinButtonValue := val]

    label ← labelNew $ Just "Notes per octave:"

    box ← vBoxNew False 5
    containerAdd box label
    containerAdd box btn

    connectGeneric "value-changed" True btn $ \_ → do
      x ← fromIntegral <$> spinButtonGetValueAsInt btn
      setNotesPerOctaveHandler ctx $ toNotesPerOctave x
      pure (0 ∷ CInt)

    pure (box, spinButtonSetValue btn ∘ fromIntegral ∘ fromNotesPerOctave)

  topButtons ← do
    box ← hBoxNew False 5
    containerAdd box panicEl
    containerAdd box channelEl
    containerAdd box baseKeyEl
    containerAdd box exitEl
    pure box

  topNumberBoxes ← do
    box ← hBoxNew False 5
    containerAdd box basePitchEl
    containerAdd box baseOctaveEl
    containerAdd box notesPerOctaveEl
    containerAdd box octaveEl
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
    containerAdd box topNumberBoxes
    containerAdd box keyboardFrame
    pure box

  containerAdd wnd mainBox
  widgetShowAll wnd

  let buttonsMap ∷ HashMap RowKey (Button, String → IO ())
      buttonsMap = fromList allButtons

      updateButton
        ∷ Pitch → HashMap RowKey Pitch
        → Octave → NotesPerOctave
        → (RowKey, (Button, String → IO ())) → IO ()

      updateButton basePitch pitchMapping octave perOctave (rowKey, (btn, labelUpdater)) = do
        let keyLabel = keyLabelMap ! rowKey

            (btnLabel, className) =
              getButtonLabelAndClass basePitch pitchMapping octave perOctave rowKey keyLabel

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

        forM_ allButtons $ updateButton (guiStateBasePitch s) (guiStatePitchMapping s)
                                        (guiStateOctave s)    (guiStateNotesPerOctave s)

  void $ forkIO $ catchThreadFail [] "GUI listener for GUI state updates" $ forever $
    takeMVar stateUpdateBus >>= \case
      SetBaseKey k → do
        modifyIORef guiStateRef $ \s → s { guiStateBaseKey = k }
        postGUIAsync $ baseKeyUpdater k >> updateButtons

      SetBasePitch p → do
        modifyIORef guiStateRef $ \s → s { guiStateBasePitch = p }
        postGUIAsync $ basePitchUpdater p >> updateButtons

      SetOctave o → do
        modifyIORef guiStateRef $ \s → s { guiStateOctave = o }
        postGUIAsync $ octaveUpdater o >> updateButtons

      SetBaseOctave o → do
        modifyIORef guiStateRef $ \s → s { guiStateBaseOctave = o }
        postGUIAsync $ baseOctaveUpdater o >> updateButtons

      SetNotesPerOctave n → do
        modifyIORef guiStateRef $ \s → s { guiStateNotesPerOctave = n }
        postGUIAsync $ notesPerOctaveUpdater n >> updateButtons

      SetPitchMapping mapping → do
        modifyIORef guiStateRef $ \s → s { guiStatePitchMapping = mapping }
        postGUIAsync updateButtons

      SetChannel ch → do
        modifyIORef guiStateRef $ \s → s { guiStateChannel = ch }
        postGUIAsync $ channelUpdater ch

      SetVelocity vel →
        modifyIORef guiStateRef $ \s → s { guiStateVelocity = vel }

      KeyButtonState rowKey isPressed →
        fromMaybe (pure ()) $ rowKey `lookup` buttonsMap <&> \(w, _) → postGUIAsync $ do
          styleContext ← widgetGetStyleContext w
          let f = if isPressed then styleContextAddClass else styleContextRemoveClass
           in f styleContext "active"

  pure wnd


myGUI ∷ GUIContext → MVar GUIStateUpdate → (Window → IO ()) → IO ()
myGUI ctx stateUpdateBus withMainWindow = do
  initGUI
  cssProvider ← getCssProvider
  mainAppWindow ctx cssProvider stateUpdateBus >>= withMainWindow
  mainGUI
  appExitHandler ctx

guiAlerts ∷ MVar AlertMessage → Window → IO ()
guiAlerts alertsBus wnd = forever $
  takeMVar alertsBus >>=
    \case InfoAlert  msg → showDialog MessageInfo  msg
          ErrorAlert msg → showDialog MessageError msg
  where
    showDialog ∷ MessageType → Text → IO ()
    showDialog msgType msg = postGUIAsync $ do
      w ← messageDialogNew (Just wnd) dialogFlags msgType ButtonsOk msg
      _ ← dialogRun w
      widgetDestroy w

    dialogFlags = [DialogModal, DialogDestroyWithParent]

runGUI ∷ GUIContext → IO GUIInterface
runGUI ctx = do
  (stateUpdateBus ∷ MVar GUIStateUpdate) ← newEmptyMVar
  (alertsBus      ∷ MVar AlertMessage)   ← newEmptyMVar

  let withMainWindow ∷ Window → IO ()
      withMainWindow =
        void ∘ forkIO ∘ catchThreadFail [MVarInfLockIsOkay] "GUI Alerts" ∘ guiAlerts alertsBus

  _ ← forkIO $ catchThreadFail [] "Main GUI" $ myGUI ctx stateUpdateBus withMainWindow

  pure GUIInterface { guiStateUpdate = putMVar stateUpdateBus
                    , guiShowAlert   = putMVar alertsBus
                    }


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
