{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module MIDIPlayer where

import Prelude.Unicode
import GHC.TypeLits

import           Data.Proxy
import           Data.ByteString.Lazy.Char8 as BS

import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.MVar

import           Sound.MIDI.Message as MMsg
import           Sound.MIDI.Message.Channel as MCh
import qualified Sound.MIDI.Message.Channel.Voice as MVo

import           System.IO as IO
import           System.Process

-- local
import Types
import Utils


type MIDIPlayerBus    = MVar MIDIPlayerAction
type MIDIPlayerSender = MIDIPlayerAction → IO ()

data MIDIPlayerAction
  = NoteOn  Channel Pitch Velocity
  | NoteOff Channel Pitch Velocity
  | Panic
  deriving (Show, Eq)


runMIDIPlayer ∷ IO MIDIPlayerSender
runMIDIPlayer = do
  (bus ∷ MIDIPlayerBus) ← newEmptyMVar

  (putMVar bus <$) $ forkIO $ catchThreadFail "MIDI Player" $ do
    (Just inHdl, _, _, !_) ← createProcess (proc "./midiplayer" []) {std_in = CreatePipe}
    hSetBuffering inHdl NoBuffering
    hSetBinaryMode inHdl True

    let printEv ev = do IO.hPrint inHdl $ BS.length ev -- Print size as line
                        BS.hPut inHdl ev               -- Print bytes of an event
                        IO.hPutStrLn inHdl ""          -- Terminate an event with empty line

    forever $ action2midi <$> takeMVar bus >>= \case

      [toBSEvent → ev] → do
        IO.hPutStrLn inHdl "single"
        printEv ev

      events → do
        IO.hPutStrLn inHdl "multiple"
        IO.hPrint inHdl $ Prelude.length events
        forM_ events $ toBSEvent • printEv
        IO.hPutStrLn inHdl "" -- Terminate "multiple" command with an empty line


action2midi ∷ MIDIPlayerAction → [MCh.T]
action2midi = \case
  NoteOn  ch note vel → [MCh.Cons ch $ Voice $ MVo.NoteOn  note vel]
  NoteOff ch note vel → [MCh.Cons ch $ Voice $ MVo.NoteOff note vel]
  Panic               → panic

-- Send note-off for every note and for every channel.
panic ∷ [MCh.T]
panic = [ MCh.Cons ch $ Voice $ MVo.NoteOff note MVo.normalVelocity
        | ch   ← [(minBound ∷ MCh.Channel) .. maxBound]
        , note ← [(minBound ∷ MVo.Pitch)   .. maxBound]
        ]

toBSEvent ∷ MCh.T → BS.ByteString
toBSEvent = MMsg.Channel • toByteString
