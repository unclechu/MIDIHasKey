{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude.Unicode

import Data.Proxy

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import Sound.MIDI.Message.Channel
import Sound.MIDI.Message.Channel.Voice (normalVelocity)

-- local
import Utils
import GUI
import Types
import MIDIPlayer
import HandleKeyboard
import EventHandler
import Keys.Types


main = do
  (appExitBus        ∷ MVar ())             ← newEmptyMVar
  (guiStateUpdateBus ∷ MVar GUIStateUpdate) ← newEmptyMVar

  evIface ← runMIDIPlayer >>= \sendToMP →

    let evListener (KeyPress   key) _ = putMVar guiStateUpdateBus $ KeyButtonState key True
        evListener (KeyRelease key) _ = putMVar guiStateUpdateBus $ KeyButtonState key False

        evListener (NewBaseKey k) s = do
          putMVar guiStateUpdateBus $ SetBaseKey k
          putMVar guiStateUpdateBus $ SetPitchMapping $ pitchMap s

        evListener (NewBasePitch p) s = do
          putMVar guiStateUpdateBus $ SetBasePitch p
          putMVar guiStateUpdateBus $ SetPitchMapping $ pitchMap s

        evListener (NewOctave o) s = do
          putMVar guiStateUpdateBus $ SetOctave o
          putMVar guiStateUpdateBus $ SetPitchMapping $ pitchMap s

        evListener (NewChannel ch)  _ = putMVar guiStateUpdateBus $ SetChannel ch
        evListener (NewVelocity v)  _ = putMVar guiStateUpdateBus $ SetVelocity v
        evListener _                _ = pure ()

     in runEventHandler EventHandlerContext { sendToMIDIPlayer = sendToMP
                                            , eventsListener   = evListener
                                            , onNewAppState    = const $ pure () -- TODO
                                            }

  let sendToEventHandler = handleEvent evIface

      keyHandler ∷ RowKey → Bool → IO ()
      keyHandler rowKey isPressed =
        let ev = if isPressed then KeyPress rowKey else KeyRelease rowKey
         in void $ forkIO $ sendToEventHandler ev

  getArgs >>= \x → runKeyboardHandling HandleKeyboardContext { devices = x
                                                             , handleKeyboardKeyEvent = keyHandler
                                                             }

  guiInitValues ← getAppState evIface <&> \appState →
    GUIState { guiStateBaseKey        = baseKey        appState
             , guiStateBasePitch      = basePitch      appState
             , guiStatePitchMapping   = pitchMap       appState
             , guiStateChannel        = channel        appState
             , guiStateVelocity       = velocity       appState
             , guiStateOctave         = octave         appState
             , guiStateNotesPerOctave = notesPerOctave appState
             }

  guiIface ←
    runGUI GUIContext { appExitHandler       = void $ forkIO $ putMVar appExitBus ()
                      , panicButtonHandler   = void $ forkIO $ sendToEventHandler PanicEvent
                      , setBaseKeyHandler    = void ∘ forkIO ∘ sendToEventHandler ∘ NewBaseKey
                      , setBasePitchHandler  = void ∘ forkIO ∘ sendToEventHandler ∘ NewBasePitch
                      , selectChannelHandler = void ∘ forkIO ∘ sendToEventHandler ∘ NewChannel
                      , setOctaveHandler     = void ∘ forkIO ∘ sendToEventHandler ∘ NewOctave
                      , noteButtonHandler    = keyHandler
                      , initialValues        = guiInitValues
                      }

  void $ forkIO $ catchThreadFail "Main module listener for GUI state updates" $ forever $
    takeMVar guiStateUpdateBus >>= guiStateUpdate guiIface

  takeMVar appExitBus
  hPutStrLn stderr "Application is terminating…"
