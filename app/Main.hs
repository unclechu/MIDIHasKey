{-# LANGUAGE UnicodeSyntax #-}

import Prelude.Unicode

import Data.Proxy
import Data.Function

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

import Sound.MIDI.Message.Channel
import Sound.MIDI.Message.Channel.Voice (normalVelocity)

-- local
import Utils
import GUI
import MIDIPlayer
import HandleKeyboard
import Keys.Types (AllKeysRows)
import Keys.Specific.GUI (getAllGUIRows)


main = do
  let startMidiKey = toPitch 20
      allRowsList = getAllGUIRows startMidiKey (Proxy ∷ Proxy AllKeysRows)

  sendToMIDIPlayer ← runMIDIPlayer

  runGUI GUIContext { allRows = allRowsList

                    , noteButtonHandler =
                        \_ midiNote isPressed →
                          let f = if isPressed then NoteOn else NoteOff
                           in void $ forkIO $ sendToMIDIPlayer $ f midiNote normalVelocity

                    , panicButtonHandler = void $ forkIO $ sendToMIDIPlayer Panic
                    }
