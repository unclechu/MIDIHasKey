{-# LANGUAGE UnicodeSyntax #-}

import Prelude.Unicode
import Data.Proxy

import Data.Word
import Data.Function
import Control.Concurrent
import Control.Concurrent.MVar

-- local
import Utils
import Keys
import GUI
import MIDIPlayer


main = do
  let startMidiKey = 20 ∷ Word8
      allRowsList = getAllRows startMidiKey (Proxy ∷ Proxy AllRows)

  sendToMIDIPlayer ← runMIDIPlayer

  runGUI GUIContext { allRows       = allRowsList
                    , buttonHandler = \_ midiNote → sendToMIDIPlayer $ NoteOn midiNote
                    }
