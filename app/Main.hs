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
import Keys
import GUI
import MIDIPlayer


main = do
  let startMidiKey = toPitch 20
      allRowsList = getAllRows startMidiKey (Proxy ∷ Proxy AllRows)

  sendToMIDIPlayer ← runMIDIPlayer

  runGUI GUIContext { allRows = allRowsList

                    , noteButtonHandler =
                        \_ midiNote isPressed →
                          let f = if isPressed then NoteOn else NoteOff
                           in void $ forkIO $ sendToMIDIPlayer $ f midiNote normalVelocity

                    , panicButtonHandler = void $ forkIO $ sendToMIDIPlayer Panic
                    }
