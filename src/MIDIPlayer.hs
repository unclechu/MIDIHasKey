{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ViewPatterns #-}

module MIDIPlayer where

import Prelude.Unicode
import GHC.TypeLits

import           Data.Proxy
import           Data.ByteString.Lazy.Char8 as BS

import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception

import           Sound.MIDI.Message as MMsg
import           Sound.MIDI.Message.Channel as MCh
import qualified Sound.MIDI.Message.Channel.Voice as MVo

import           System.IO as IO
import           System.Process

-- local
import Types
import Utils

foreign import ccall "exit" exit ∷ IO ()

type MIDIPlayerBus = MVar MIDIPlayerAction

data MIDIPlayerAction
  = NoteOn  Pitch Velocity
  | NoteOff Pitch Velocity
  | Panic
  deriving (Show, Eq)


runMIDIPlayer ∷ IO (MIDIPlayerAction → IO ())
runMIDIPlayer = do
  (bus ∷ MIDIPlayerBus) ← newEmptyMVar

  (putMVar bus <$) $ forkIO $ protect $ do
    (Just inHdl, _, _, !_) ← createProcess (proc "midiplayer" []) {std_in = CreatePipe}
    hSetBuffering inHdl NoBuffering
    hSetBinaryMode inHdl True

    let printEv ev = do IO.hPrint inHdl $ BS.length ev -- Print size as line
                        BS.hPut inHdl ev -- Print bytes of an event
                        IO.hPutStrLn inHdl "" -- Terminate an event with empty line

    forever $ action2midi <$> takeMVar bus >>= \case

      [toBSEvent → ev] → do
        IO.hPutStrLn inHdl "single"
        printEv ev

      events → do
        IO.hPutStrLn inHdl "multiple"
        IO.hPrint inHdl $ Prelude.length events
        forM_ events $ toBSEvent • printEv
        IO.hPutStrLn inHdl "" -- Terminate "multiple" command with an empty line

  where protect = handle
                $ \(e ∷ SomeException) → do IO.hPutStrLn IO.stderr "MIDI Player thread is failed!"
                                            exit


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

toBSEvent ∷ MCh.T → BS.ByteString
toBSEvent = MMsg.Channel • toByteString
