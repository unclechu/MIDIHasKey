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
import Keys.Types


main = do
  sendToMIDIPlayer ← runMIDIPlayer
  evIface ← runEventHandler sendToMIDIPlayer

  let sendToEventHandler = handleEvent evIface

      keyHandler ∷ RowKey → Bool → IO ()
      keyHandler rowKey isPressed =
        let ev = if isPressed then KeyPress rowKey else KeyRelease rowKey
         in void $ forkIO $ sendToEventHandler ev

  getArgs >>= \x → runKeyboardHandling HandleKeyboardContext { devices = x
                                                             , handleKeyboardKeyEvent = keyHandler
                                                             }

  runGUI GUIContext { noteButtonHandler  = keyHandler
                    , panicButtonHandler = void $ forkIO $ sendToEventHandler PanicEvent
                    , getPitchMapping    = getAppState evIface <&> pitchMap
                    }
