{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MIDIPlayer where

import Prelude.Unicode
import GHC.TypeLits

import Data.Proxy
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Trans.Class (lift)

import Sound.JACK as JACK
import Sound.JACK.MIDI as JMIDI
import Sound.MIDI.Message.Channel

-- local
import Types
import Utils

type MIDIPlayerBus = MVar MIDIPlayerAction

data MIDIPlayerAction
  = NoteOn  Pitch Velocity
  | NoteOff Pitch Velocity
  deriving (Show, Eq)


runMIDIPlayer ∷ IO (MIDIPlayerAction → IO ())
runMIDIPlayer = do
  (bus ∷ MIDIPlayerBus) ← newEmptyMVar

  (putMVar bus <$) $ forkIO $ handleExceptions $ do
    client ← newClientDefault $ symbolVal (Proxy ∷ Proxy AppName)
    (port ∷ JMIDI.Port Output) ← newPort client (symbolVal (Proxy ∷ Proxy OutputPortName))

    let processCallback nframes = lift $
          tryTakeMVar bus >>= \case Nothing → pure ()
                                    Just (NoteOn  note vel) → handleNoteOn  note vel
                                    Just (NoteOff note vel) → handleNoteOff note vel

    JACK.withProcess client processCallback $ activate client


handleNoteOn ∷ Pitch → Velocity → IO ()
handleNoteOn note vel = putStrLn $ "TODO playing MIDI note on: " ⧺ show note

handleNoteOff ∷ Pitch → Velocity → IO ()
handleNoteOff note vel = putStrLn $ "TODO playing MIDI note off: " ⧺ show note
