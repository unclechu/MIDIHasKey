{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MIDIPlayer where

import Prelude.Unicode
import GHC.TypeLits

import Data.Proxy
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Exception.Synchronous
import Control.Concurrent
import Control.Concurrent.MVar

import Sound.JACK as JACK
import Sound.JACK.MIDI as JMIDI
import Sound.JACK.Exception
import Sound.MIDI.Message as MMsg
import Sound.MIDI.Message.Channel as MCh
import qualified Sound.MIDI.Message.Channel.Voice as MVo

-- local
import Types
import Utils

type MIDIPlayerBus = MVar MIDIPlayerAction
type MMonad e a = ExceptionalT e IO a
type MTask e = MMonad e (Maybe MIDIPlayerAction)

data MIDIPlayerAction
  = NoteOn  Pitch Velocity
  | NoteOff Pitch Velocity
  deriving (Show, Eq)


runMIDIPlayer ∷ IO (MIDIPlayerAction → IO ())
runMIDIPlayer = do
  (bus ∷ MIDIPlayerBus) ← newEmptyMVar

  (putMVar bus <$) $ forkIO $ handleExceptions $ do
    client ← newClientDefault $ symbolVal (Proxy ∷ Proxy AppName)
    (port ∷ JMIDI.Port Output) ← newPort client $ symbolVal (Proxy ∷ Proxy OutputPortName)

    let getTask = lift $ tryTakeMVar bus ∷ MTask e

        getTasks ∷ [MIDIPlayerAction] → MMonad e [MIDIPlayerAction]
        getTasks acc = getTask >>= \case Nothing → pure acc
                                         Just x  → getTasks $ x : acc

        getPortBuf = getBuffer port >=> \x → x <$ clearBuffer x

        processCallback ∷ ThrowsErrno e ⇒ NFrames → MMonad e ()
        processCallback (lift ∘ getPortBuf → getBuf)
          = getTasks []
            <&> map (action2midi • MMsg.Channel)
            >>= \case [] → () <$ getBuf -- Just clearing the buffer.
                                        -- Or it triggers old event every iteration.
                      list → do buf ← getBuf
                                forM_ list $ writeEvent buf $ NFrames 0

    JACK.withProcess client processCallback $ activate client


action2midi ∷ MIDIPlayerAction → MCh.T
action2midi = \case
  NoteOn  note vel → MCh.Cons (toChannel 0) $ Voice $ MVo.NoteOn  note vel
  NoteOff note vel → MCh.Cons (toChannel 0) $ Voice $ MVo.NoteOff note vel
