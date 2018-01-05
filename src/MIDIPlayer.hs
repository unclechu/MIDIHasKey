{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MIDIPlayer where

import Prelude.Unicode

import Data.Word
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

import Sound.JACK.MIDI

-- local
import Utils

type MIDIPlayerBus = MVar MIDIPlayerAction

data MIDIPlayerAction
  = NoteOn  Word8
  | NoteOff Word8
  deriving (Show, Eq)


runMIDIPlayer ∷ IO (MIDIPlayerAction → IO ())
runMIDIPlayer = do
  (bus ∷ MIDIPlayerBus) ← newEmptyMVar
  (putMVar bus <$) $ forkIO $ forever $
    takeMVar bus >>= \case NoteOn  x → handleNoteOn  x
                           NoteOff x → handleNoteOff x


handleNoteOn ∷ Word8 → IO ()
handleNoteOn note = putStrLn $ "TODO playing MIDI note on: " ⧺ show note

handleNoteOff ∷ Word8 → IO ()
handleNoteOff note = putStrLn $ "TODO playing MIDI note off: " ⧺ show note


-- main = mainStereo $ pure ∘ (process *** process)
-- process ∷ Sample → Sample
-- process = id
