{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- A module that transforms some events such as key press/release to MIDI events.
-- It also stores some application state such as current base note, current channel, etc., and
-- handles changes of this state and provides API to get current state.
module EventHandler
     ( runEventHandler
     , EventToHandle (..)
     , EventHandlerInterface (..)
     ) where

import Prelude.Unicode

import Data.IORef

import Control.Monad
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)

import Sound.MIDI.Message.Channel
import Sound.MIDI.Message.Channel.Voice (normalVelocity)

-- local
import Utils
import Keys.Types
import MIDIPlayer


type EventHandlerBus    = MVar EventToHandle
type EventHandlerSender = EventToHandle → IO ()

data EventToHandle
  = KeyPress   RowKey
  | KeyRelease RowKey

  | NewBasePitch Pitch
  | NewChannel   Channel
  | NewVelocity  Velocity

  | PanicEvent
  deriving (Show, Eq)

data EventHandlerInterface
  = EventHandlerInterface
  { handleEvent ∷ EventHandlerSender
  , getAppState ∷ IO AppState
  }

data AppState
  = AppState
  { basePitch ∷ Pitch
  , channel   ∷ Channel
  , velocity  ∷ Velocity
  } deriving (Show, Eq)


runEventHandler ∷ MIDIPlayerSender → IO EventHandlerInterface
runEventHandler sendToMP = do
  (bus ∷ EventHandlerBus) ← newEmptyMVar

  (appStateRef ∷ IORef AppState) ← newIORef AppState { basePitch = toPitch 20
                                                     , channel   = toChannel 0
                                                     , velocity  = normalVelocity
                                                     }

  let interface
        = EventHandlerInterface
        { handleEvent = putMVar bus
        , getAppState = readIORef appStateRef
        }

      handle (KeyPress k) = do appState ← readIORef appStateRef
                               sendToMP $ NoteOn (channel appState)
                                                 (basePitch appState)
                                                 -- ^ TODO get pitch relative to key
                                                 (velocity appState)

      handle (KeyRelease k) = do appState ← readIORef appStateRef
                                 sendToMP $ NoteOff (channel appState)
                                                    (basePitch appState)
                                                    -- ^ TODO get pitch relative to key
                                                    (velocity appState)

      handle (NewBasePitch p) = modifyIORef appStateRef $ \s → s { basePitch = p }
      handle (NewChannel c)   = modifyIORef appStateRef $ \s → s { channel   = c }
      handle (NewVelocity v)  = modifyIORef appStateRef $ \s → s { velocity  = v }

      handle PanicEvent = sendToMP Panic

  (interface <$) $ forkIO $ catchThreadFail "Event Handler" $ forever $ takeMVar bus >>= handle
