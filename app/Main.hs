{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude.Unicode

import Data.Proxy
import Data.Function

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

import System.Environment (getArgs)

import Sound.MIDI.Message.Channel
import Sound.MIDI.Message.Channel.Voice (normalVelocity)

-- local
import Utils
import GUI
import MIDIPlayer
import HandleKeyboard
import EventHandler
import Keys.Types


main = do
  (appExitBus  ∷ MVar ())             ← newEmptyMVar
  (keyStateBus ∷ MVar (RowKey, Bool)) ← newEmptyMVar

  evIface ← runMIDIPlayer >>= \sendToMP →

    let evListener (KeyPress   key) = putMVar keyStateBus (key, True)
        evListener (KeyRelease key) = putMVar keyStateBus (key, False)
        evListener _                = pure ()

     in runEventHandler EventHandlerContext { sendToMIDIPlayer = sendToMP
                                            , eventsListener   = evListener
                                            }

  let sendToEventHandler = handleEvent evIface

      keyHandler ∷ RowKey → Bool → IO ()
      keyHandler rowKey isPressed =
        let ev = if isPressed then KeyPress rowKey else KeyRelease rowKey
         in void $ forkIO $ sendToEventHandler ev

  getArgs >>= \x → runKeyboardHandling HandleKeyboardContext { devices = x
                                                             , handleKeyboardKeyEvent = keyHandler
                                                             }

  guiIface ←
    runGUI GUIContext { getPitchMapping    = getAppState evIface <&> pitchMap
                      , appExitHandler     = void $ forkIO $ putMVar appExitBus ()
                      , panicButtonHandler = void $ forkIO $ sendToEventHandler PanicEvent
                      , noteButtonHandler  = keyHandler
                      }

  void $ forkIO $ catchThreadFail "Main module listener for key state updates" $ forever $
    takeMVar keyStateBus >>= uncurry (keyButtonStateUpdate guiIface)

  takeMVar appExitBus
