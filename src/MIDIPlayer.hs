{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module MIDIPlayer where

import Prelude.Unicode
import GHC.TypeLits

import Data.Proxy
import Data.Foldable
import Data.Function
import Data.List
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
type NextIterBus = MVar [MMsg.T]
type MMonad e a = ExceptionalT e IO a

data MIDIPlayerAction
  = NoteOn  Pitch Velocity
  | NoteOff Pitch Velocity
  | Panic
  deriving (Show, Eq)


runMIDIPlayer ∷ IO (MIDIPlayerAction → IO ())
runMIDIPlayer = do
  (bus ∷ MIDIPlayerBus) ← newEmptyMVar
  (nextIterBus ∷ NextIterBus) ← newEmptyMVar

  (putMVar bus <$) $ forkIO $ handleExceptions $ do
    client ← newClientDefault $ symbolVal (Proxy ∷ Proxy AppName)
    (port ∷ JMIDI.Port Output) ← newPort client $ symbolVal (Proxy ∷ Proxy OutputPortName)

    let -- Recursively get all tasks we have at the moment and build MIDI events from them.
        getEvents ∷ MMonad e [MMsg.T]
        getEvents = m []
          where m acc = lift (tryTakeMVar bus)
                  >>= \case Nothing → pure $ reverse acc -- Getting correct order by reversing
                            Just x  → m $ f acc $ action2midi x

                f = foldl $ \acc x → MMsg.Channel x : acc

        getEventsFromPrevIter ∷ MMonad e [MMsg.T]
        getEventsFromPrevIter = m []
          where m acc = lift (tryTakeMVar nextIterBus)
                  >>= \case Nothing → pure acc
                            Just x  → m $ acc ⧺ x

        getPortBuf ∷ NFrames → IO (Buffer Output)
        getPortBuf = getBuffer port >=> \x → x <$ clearBuffer x

        processCallback ∷ ThrowsErrno e ⇒ NFrames → MMonad e ()
        processCallback nframes@(NFrames bufSize) = do
          buf           ← lift $ getPortBuf nframes
          events        ← getEvents
          delayedEvents ← getEventsFromPrevIter

          let (oldForThisIter, oldForNextIter) = genericSplitAt bufSize delayedEvents
              (forThisIter, forNextIter)       = genericSplitAt (bufSize - oldFrames) events

              oldFrames = genericLength oldForThisIter

              reducer event frame = frame + 1 <$ writeEvent buf (NFrames frame) event

          foldrM reducer 0 oldForThisIter >>= flip (foldrM reducer) forThisIter

          when (genericLength oldForNextIter > 0 || genericLength forNextIter > 0) $
            lift $ putMVar nextIterBus $ oldForNextIter ⧺ forNextIter

    JACK.withProcess client processCallback $ activate client


action2midi ∷ MIDIPlayerAction → [MCh.T]
action2midi = \case
  NoteOn  note vel → [MCh.Cons (toChannel 0) $ Voice $ MVo.NoteOn  note vel]
  NoteOff note vel → [MCh.Cons (toChannel 0) $ Voice $ MVo.NoteOff note vel]
  Panic            → panic

-- Send note-off for every note and for every channel.
panic ∷ [MCh.T]
panic = [ MCh.Cons ch $ Voice $ MVo.NoteOff note MVo.normalVelocity
        | ch   ← [(minBound ∷ MCh.Channel) .. maxBound]
        , note ← [(minBound ∷ MVo.Pitch)   .. maxBound]
        ]
