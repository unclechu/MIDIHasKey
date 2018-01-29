{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- A module that transforms some events such as key press/release to MIDI events.
-- It also stores some application state such as current base note, current channel, etc., and
-- handles changes of this state and provides API to get current state.
module EventHandler
     ( runEventHandler
     , EventHandlerContext (..)
     , EventHandlerInterface (..)
     , AppState (..)
     , EventToHandle (..)
     ) where

import Prelude hiding (lookup)
import Prelude.Unicode

import Data.IORef
import Data.HashMap.Strict

import Control.Monad
import Control.Arrow
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)

import Sound.MIDI.Message.Channel
import Sound.MIDI.Message.Channel.Voice (normalVelocity)

-- local
import Utils
import Keys.Types
import Keys.Specific.EventHandler
import MIDIPlayer


data EventHandlerContext
  = EventHandlerContext
  { sendToMIDIPlayer ∷ MIDIPlayerSender
  , eventsListener   ∷ EventsListener
  }

data EventHandlerInterface
  = EventHandlerInterface
  { handleEvent ∷ EventHandlerSender
  , getAppState ∷ IO AppState
  }


data AppState
  = AppState
  { baseKey   ∷ RowKey
  , basePitch ∷ Pitch
  , channel   ∷ Channel
  , velocity  ∷ Velocity
  , pitchMap  ∷ HashMap RowKey Pitch
  , storedEv  ∷ HashMap RowKey StoredEvent
  } deriving (Show, Eq)

data EventToHandle
  = KeyPress   RowKey
  | KeyRelease RowKey

  | NewBasePitch Pitch
  | NewChannel   Channel
  | NewVelocity  Velocity

  | PanicEvent

  deriving (Show, Eq)


type EventHandlerBus    = MVar EventToHandle
type EventHandlerSender = EventToHandle → IO ()
type EventsListener     = EventToHandle → IO ()


-- We need this to trigger correct note-off when channel/base-key/base-pitch/channel is changed
-- before last triggered note is released.
data StoredEvent
  = StoredNoteOn Channel Pitch Velocity
  deriving (Show, Eq)


defaultAppState ∷ AppState
defaultAppState =

  AppState { baseKey   = baseKey'
           , basePitch = basePitch'
           , channel   = toChannel 0
           , velocity  = normalVelocity
           , pitchMap  = getPitchMapping baseKey' basePitch'
           , storedEv  = empty
           }

  where baseKey'   = AKey
        basePitch' = toPitch 19 -- 20th in [1..128]


runEventHandler ∷ EventHandlerContext → IO EventHandlerInterface
runEventHandler ctx = do
  (bus ∷ EventHandlerBus) ← newEmptyMVar

  (appStateRef ∷ IORef AppState) ← newIORef defaultAppState

  let interface
        = EventHandlerInterface
        { handleEvent = putMVar bus
        , getAppState = readIORef appStateRef
        }

      handle (KeyPress k) = do
        appState ← readIORef appStateRef

        case lookup k $ pitchMap appState of

             Just p → do let ch     = channel appState
                             vel    = velocity appState
                             stored = StoredNoteOn ch p vel

                         sendToMIDIPlayer ctx $ NoteOn ch p vel
                         updateState $ \s → s { storedEv = insert k stored (storedEv s) }

             Nothing → pure ()

      handle (KeyRelease k) = readIORef appStateRef >>= \appState → eitherValue $ do

        case lookup k $ storedEv appState of

             Just (StoredNoteOn ch p vel) → Left $ do
               sendToMIDIPlayer ctx $ NoteOff ch p vel
               updateState $ \s → s { storedEv = k `delete` storedEv s }

             Nothing → Right ()

        pure $
          case lookup k $ pitchMap appState of
               Just x  → sendToMIDIPlayer ctx $ NoteOff (channel appState) x $ velocity appState
               Nothing → pure ()

      handle (NewBasePitch p) = updateState $ \s → s { basePitch = p }
      handle (NewChannel c)   = updateState $ \s → s { channel   = c }
      handle (NewVelocity v)  = updateState $ \s → s { velocity  = v }

      handle PanicEvent = sendToMIDIPlayer ctx Panic

      updateState = modifyIORef' appStateRef ∘ updateStateMiddleware

  (interface <$) $ forkIO $ catchThreadFail "Event Handler" $ forever $
    let notifyListener = forkIO ∘ catchThreadFail "Events listener notifier" ∘ eventsListener ctx
     in takeMVar bus >>= (handle &&& notifyListener) • uncurry (>>)

  where updateStateMiddleware ∷ (AppState → AppState) → AppState → AppState
        updateStateMiddleware f oldState =

          if baseKey   newState /= baseKey   oldState
          || basePitch newState /= basePitch oldState
             then newState { pitchMap = getPitchMapping baseKey' basePitch' }
             else newState

          where newState   = f oldState
                baseKey'   = baseKey newState
                basePitch' = basePitch newState


getPitchMapping ∷ RowKey → Pitch → HashMap RowKey Pitch
getPitchMapping baseKey basePitch = fromList (zip l lp) `union` fromList (zip r rp)

  where (reverse → l, r) = span (/= baseKey) allKeysOrder

        lp, rp ∷ [Pitch]

        lp = eitherValue $ do
          if basePitch > minBound then Right () else Left []
          let prev = pred basePitch
          if prev > minBound then Right [prev, pred prev .. minBound] else Left [prev]

        rp = eitherValue $ do
          if maxBound > basePitch then Right () else Left [basePitch]
          let next = succ basePitch
          if maxBound > next then Right [basePitch .. maxBound] else Left [basePitch, next]


-- For extracting value from breakable monads
eitherValue ∷ Either a a → a
eitherValue (Left  x) = x
eitherValue (Right x) = x
