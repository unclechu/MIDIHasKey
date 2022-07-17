{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where -- "midihaskey" app

import Prelude.Unicode

import Data.Default (def)
import Text.InterpolatedString.QM

import Control.Monad
import Control.Concurrent

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

-- local
import MIDIHasKey.Utils
import Types
import GUI
import MIDIPlayer
import HandleKeyboard
import EventHandler
import Keys.Types
import MIDIHasKey.Config


main ∷ IO ()
main = do
  (appExitBus        ∷ MVar ())             ← newEmptyMVar
  (guiStateUpdateBus ∷ MVar GUIStateUpdate) ← newEmptyMVar
  (alertsBus         ∷ MVar AlertMessage)   ← newEmptyMVar

  !(config ∷ Config) ←
    readConfig >>= \case
      Right config → pure config
      Left  errMsg → def <$ alertsBus `putMVar`
        ErrorAlert [qms| Parsing config failed with message: {errMsg} |]

  evIface ← runMIDIPlayer >>= \sendToMP →

    let evListener ∷ EventToHandle → AppState → IO ()

        evListener (KeyPress   key) _ = guiUpdate $ KeyButtonState key True
        evListener (KeyRelease key) _ = guiUpdate $ KeyButtonState key False

        evListener (NewBaseKey k) s = do
          guiUpdate $ SetBaseKey k
          guiUpdate $ SetPitchMapping $ pitchMap s

        evListener (NewBasePitch p) s = do
          guiUpdate $ SetBasePitch p
          guiUpdate $ SetPitchMapping $ pitchMap s

        evListener (NewOctave o) s = do
          guiUpdate $ SetOctave o
          guiUpdate $ SetPitchMapping $ pitchMap s

        evListener (NewBaseOctave o) s = do
          guiUpdate $ SetBaseOctave o
          guiUpdate $ SetPitchMapping $ pitchMap s

        evListener (NewNotesPerOctave n) s = do
          guiUpdate $ SetNotesPerOctave n
          guiUpdate $ SetPitchMapping $ pitchMap s

        evListener (NewChannel ch)  _ = guiUpdate $ SetChannel ch
        evListener (NewVelocity v)  _ = guiUpdate $ SetVelocity v
        evListener SaveConfig       s = guiUpdate $ NewLastSavedState $ getGUIState s

        evListener _ _ = pure ()

        guiUpdate = putMVar guiStateUpdateBus

     in runEventHandler EventHandlerContext { sendToMIDIPlayer = sendToMP
                                            , eventsListener   = evListener
                                            , onNewAppState    = const $ pure () -- TODO
                                            , initialConfig    = config
                                            }

  let sendToEventHandler = handleEvent evIface

      keyHandler ∷ RowKey → 𝔹 → IO ()
      keyHandler rowKey isPressed =
        let ev = if isPressed then KeyPress rowKey else KeyRelease rowKey
         in void $ forkIO $ sendToEventHandler ev

  getArgs >>= \x → runKeyboardHandling HandleKeyboardContext { devices = x
                                                             , handleKeyboardKeyEvent = keyHandler
                                                             }

  guiInitValues ← getAppState evIface <&> getGUIState

  guiIface ←
    runGUI GUIContext { initialState             = guiInitValues
                      , appExitHandler           = void $ forkIO $ putMVar appExitBus ()
                      , panicButtonHandler       = void $ forkIO $ sendToEventHandler PanicEvent
                      , saveConfigButtonHandler  = void $ forkIO $ sendToEventHandler SaveConfig

                      , setBaseKeyHandler        = void ∘ forkIO ∘ sendToEventHandler ∘ NewBaseKey
                      , setBasePitchHandler      = void ∘ forkIO ∘ sendToEventHandler ∘ NewBasePitch
                      , setOctaveHandler         = void ∘ forkIO ∘ sendToEventHandler ∘ NewOctave

                      , setBaseOctaveHandler     = void ∘ forkIO ∘ sendToEventHandler
                                                 ∘ NewBaseOctave

                      , setNotesPerOctaveHandler = void ∘ forkIO ∘ sendToEventHandler
                                                 ∘ NewNotesPerOctave

                      , selectChannelHandler     = void ∘ forkIO ∘ sendToEventHandler ∘ NewChannel

                      , noteButtonHandler        = keyHandler
                      }

  void $ forkIO $ catchThreadFail [] "Main module listener for GUI state updates" $ forever $
    takeMVar guiStateUpdateBus >>= guiStateUpdate guiIface

  -- void $ forkIO $ catchThreadFail [] "Main module listener for alerts bus" $ forever $
  --   takeMVar alertsBus >>= guiShowAlert guiIface
  -- It's commented because it fails with `BlockedIndefinitelyOnMVar` exception.
  tryTakeMVar alertsBus >>= maybeMUnit (guiShowAlert guiIface)

  takeMVar appExitBus
  hPutStrLn stderr "Application is terminating…"


getGUIState ∷ AppState → GUIState
getGUIState appState
  = GUIState
  { guiStateBaseKey        = baseKey        (appState ∷ AppState)
  , guiStateBasePitch      = basePitch      (appState ∷ AppState)
  , guiStateOctave         = octave         (appState ∷ AppState)
  , guiStateBaseOctave     = baseOctave     (appState ∷ AppState)
  , guiStateNotesPerOctave = notesPerOctave (appState ∷ AppState)

  , guiStatePitchMapping   = pitchMap       (appState ∷ AppState)

  , guiStateChannel        = channel        (appState ∷ AppState)
  , guiStateVelocity       = velocity       (appState ∷ AppState)
  }
