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

    let evListener (KeyPress   key) = putMVar guiStateUpdateBus $ KeyButtonState key True
        evListener (KeyRelease key) = putMVar guiStateUpdateBus $ KeyButtonState key False
        evListener (NewChannel ch)  = putMVar guiStateUpdateBus $ ChannelChange ch
        evListener _                = pure ()

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
    GUIInitialValues { initialBaseKey        = baseKey        appState
                     , initialBasePitch      = basePitch      appState
                     , initialPitchMapping   = pitchMap       appState
                     , initialChannel        = channel        appState
                     , initialVelocity       = velocity       appState
                     , initialOctave         = octave         appState
                     , initialNotesPerOctave = notesPerOctave appState
                     }

  guiIface ←
    runGUI GUIContext { appExitHandler       = void $ forkIO $ putMVar appExitBus ()
                      , panicButtonHandler   = void $ forkIO $ sendToEventHandler PanicEvent
                      , selectChannelHandler = void ∘ forkIO ∘ sendToEventHandler ∘ NewChannel
                      , noteButtonHandler    = keyHandler
                      , initialValues        = guiInitValues
                      }

  void $ forkIO $ catchThreadFail "Main module listener for GUI state updates" $ forever $
    takeMVar guiStateUpdateBus >>= guiStateUpdate guiIface

  takeMVar appExitBus
  hPutStrLn stderr "Application is terminating…"
