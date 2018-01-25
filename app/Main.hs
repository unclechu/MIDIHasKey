{-# LANGUAGE UnicodeSyntax #-}

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
import Keys.Types (AllKeysRows)
import Keys.Specific.GUI (getAllGUIRows)


main = do
  let startMidiKey = toPitch 20
      allRowsList = getAllGUIRows startMidiKey (Proxy ∷ Proxy AllKeysRows)

  sendToMIDIPlayer ← runMIDIPlayer
  evIface ← runEventHandler sendToMIDIPlayer

  let sendToEventHandler = handleEvent evIface

  getArgs >>= \x → runKeyboardHandling HandleKeyboardContext { devices = x }

  runGUI GUIContext { allRows = allRowsList

                    , noteButtonHandler =
                        \rowKey isPressed →
                          let ev = if isPressed then KeyPress rowKey else KeyRelease rowKey
                           in void $ forkIO $ sendToEventHandler ev

                    , panicButtonHandler = void $ forkIO $ sendToEventHandler PanicEvent
                    }
