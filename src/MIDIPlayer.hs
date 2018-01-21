{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module MIDIPlayer where

import Prelude.Unicode
import GHC.TypeLits

import           Data.Proxy
import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.Word
import           Data.IORef
import           Data.Array.IO

import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Exception.Synchronous
import           Control.Concurrent
import           Control.Concurrent.MVar

import           Sound.JACK as JACK
import           Sound.JACK.MIDI as JMIDI
import           Sound.JACK.Exception
import           Sound.MIDI.Message as MMsg
import           Sound.MIDI.Message.Channel as MCh
import qualified Sound.MIDI.Message.Channel.Voice as MVo

-- local
import Types
import Utils

type MIDIPlayerBus = MVar MIDIPlayerAction
type MMonad e a = ExceptionalT e IO a

data MIDIPlayerAction
  = NoteOn  Pitch Velocity
  | NoteOff Pitch Velocity
  | Panic
  deriving (Show, Eq)

data MIDIEvents
  = SingleEv MCh.T
  | PanicEv

data MIDIMultipleEvent
  = NoEvent
  | PanicMultipleEv NFrames -- `NFrames` is an offset


runMIDIPlayer ∷ IO (MIDIPlayerAction → IO ())
runMIDIPlayer = do
  (bus ∷ MIDIPlayerBus) ← newEmptyMVar

  (putMVar bus <$) $ forkIO $ handleExceptions $ do

    -- Build an array of panic events (for performance issues)
    (panicArr ∷ IOArray Word32 MMsg.T) ← lift $ newListArray (1, genericLength panic) panic

    -- For delayed multiple events when all events couldn't be handled in one iteration.
    -- IORef for performance issues.
    (multipleEv ∷ IORef MIDIMultipleEvent) ← lift $ newIORef NoEvent

    client ← newClientDefault $ symbolVal (Proxy ∷ Proxy AppName)
    (port ∷ JMIDI.Port Output) ← newPort client $ symbolVal (Proxy ∷ Proxy OutputPortName)

    -- WARNING! Real-time critical
    let getPortBuf ∷ NFrames → IO (Buffer Output)
        getPortBuf = getBuffer port >=> \x → x <$ clearBuffer x

        panicLen = genericLength panic ∷ Word32

        handleNewEv ∷ ThrowsErrno e
                    ⇒ NFrames -- ^ Buffer size
                    → Buffer Output
                    → NFrames -- ^ Start frame (when some already triggered in this buffer)
                    → MMonad e ()

        handleNewEv nframes@(NFrames nframesN) buf frame@(NFrames frameN) =
          fmap action2midi <$> lift (tryTakeMVar bus) >>= \case
          Nothing → pure ()

          Just (SingleEv ev) → do
            writeEvent buf frame $ MMsg.Channel ev
            let nextFrame = succNFrames frame

            if nextFrame >= nframes -- Limit of current buffer size is exceeded
               then pure () -- No more handling for current buffer
               else handleNewEv nframes buf nextFrame

          Just PanicEv → do
            let freeFramesLeft = restFrames - takenFrames -- For next recursive handle iteration
                restFrames     = nframesN - frameN -- How many free frames left in current buffer
                nextFrame      = NFrames $ frameN + takenFrames
                delayOffset    = NFrames takenFrames

                takenFrames    = min restFrames panicLen -- How many we're going to take from panic
                                                         -- array for current buffer.

                writeN ∷ ThrowsErrno e ⇒ Word32 → MMonad e ()
                writeN n
                  | nextN >= takenFrames = ev
                  | otherwise = ev >> writeN nextN
                  where nextN = succ n
                        atFrame = NFrames $ frameN + n
                        ev = lift (readArray panicArr nextN) >>= writeEvent buf atFrame

            writeN 0

            if takenFrames /= panicLen -- Some panic events haven't fit buffer size
               then lift $ atomicWriteIORef multipleEv $ PanicMultipleEv delayOffset -- Delay rest
               else if freeFramesLeft > 0 -- We have some free frames left in current buffer
                       then handleNewEv nframes buf nextFrame
                       else pure () -- Nothing more for current buffer

        handleOldPanic ∷ ThrowsErrno e
                       ⇒ NFrames -- ^ Buffer size
                       → Buffer Output
                       → NFrames -- ^ How many skip from panic array of already triggered events
                       → MMonad e ()

        handleOldPanic nframes@(NFrames nframesN) buf (NFrames skipOldN) = do

          let takenFrames     = min nframesN $ panicLen - skipOldN
              restFreeFrames  = nframesN - takenFrames
              oldLeftToHandle = panicLen - skipOldN - takenFrames
              skippedNow      = NFrames $ skipOldN + takenFrames
              nextFrame       = NFrames $ nframesN + takenFrames

              writeN ∷ ThrowsErrno e ⇒ Word32 → MMonad e ()
              writeN n
                | nextN >= takenFrames = ev
                | otherwise = ev >> writeN nextN
                where nextN = succ n
                      withSkipped = skipOldN + nextN
                      atFrame = NFrames n
                      ev = lift (readArray panicArr withSkipped) >>= writeEvent buf atFrame

          writeN 0

          if oldLeftToHandle > 0 -- We still have some to handle
             then lift $ atomicWriteIORef multipleEv $ PanicMultipleEv skippedNow
             else do lift $ atomicWriteIORef multipleEv NoEvent
                     if restFreeFrames > 0 -- We have some free frames in current buffer
                        then handleNewEv nframes buf nextFrame
                        else pure () -- Nothing more in current buffer

        processCallback ∷ ThrowsErrno e ⇒ NFrames → MMonad e ()
        processCallback nframes = do
          buf ← lift $ getPortBuf nframes

          lift (readIORef multipleEv)
            >>= \case NoEvent → handleNewEv nframes buf $ NFrames 0
                      PanicMultipleEv skipElements → handleOldPanic nframes buf skipElements

    JACK.withProcess client processCallback $ activate client


action2midi ∷ MIDIPlayerAction → MIDIEvents
action2midi = \case
  NoteOn  note vel → SingleEv $ MCh.Cons (toChannel 0) $ Voice $ MVo.NoteOn  note vel
  NoteOff note vel → SingleEv $ MCh.Cons (toChannel 0) $ Voice $ MVo.NoteOff note vel
  Panic            → PanicEv

-- Send note-off for every note and for every channel.
panic ∷ [MMsg.T]
panic = [ MMsg.Channel $ MCh.Cons ch $ Voice $ MVo.NoteOff note MVo.normalVelocity
        | ch   ← [(minBound ∷ MCh.Channel) .. maxBound]
        , note ← [(minBound ∷ MVo.Pitch)   .. maxBound]
        ]
