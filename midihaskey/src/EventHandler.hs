{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DuplicateRecordFields #-}

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

import Data.Default (def)
import Data.IORef
import Data.Maybe
import Data.List (elemIndex)
import Data.HashMap.Strict hiding (filter)
import Text.InterpolatedString.QM

import Control.Monad
import Control.Arrow
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)

import Sound.MIDI.Message.Channel
import Sound.MIDI.Message.Channel.Voice (normalVelocity)

-- local
import MIDIHasKey.Utils
import Types
import Keys.Types
import Keys.Specific.EventHandler
import MIDIPlayer
import MIDIHasKey.Config


data EventHandlerContext
   = EventHandlerContext
   { sendToMIDIPlayer ∷ MIDIPlayerSender
   , eventsListener   ∷ EventsListener
   , onNewAppState    ∷ AppState → IO ()
   , initialConfig    ∷ Config
   }

data EventHandlerInterface
   = EventHandlerInterface
   { handleEvent ∷ EventHandlerSender
   , getAppState ∷ IO AppState
   }


data AppState
   = AppState
   { baseKey        ∷ RowKey -- A key of the keyboard which would be a set point
   , basePitch      ∷ Pitch  -- A pitch that will be associated with `baseKey`
   , octave         ∷ Octave -- Current octave, it's not supposed to mean a real octave
                             -- but a shift from `basePitch` by `notesPerOctave`
                             -- relatively to the `baseOctave`.
   , baseOctave     ∷ BaseOctave -- `octave` value that would be `basePitch` on `baseKey`
                                 -- that means when `octave` equals `baseOctave` `basePitch`
                                 -- wouldn't be shifted at all.
   , notesPerOctave ∷ NotesPerOctave -- Indicates how many notes will be shifted by one `octave`,
                                     -- it does't mean anything but this shift step.
   , pitchMap       ∷ HashMap RowKey Pitch -- Current mapping of pitches by keys,
                                           -- it changes when you shift `octave`, `basePitch`, etc.
   , storedEvents   ∷ HashMap RowKey StoredEvent -- Mapping of currently triggered events such as
                                                 -- note-on to trigger proper note-off in case
                                                 -- something was shifted (`octave`, `basePitch`,
                                                 -- `channel`, etc.) while some note isn't
                                                 -- released yet.
   , channel        ∷ Channel  -- MIDI channel
   , velocity       ∷ Velocity -- MIDI velocity for the tiggered notes
   } deriving (Show, Eq)

data EventToHandle
   = KeyPress   RowKey
   | KeyRelease RowKey

   -- Constructors of new values that shifts key mapping
   | NewBaseKey        RowKey
   | NewBasePitch      Pitch
   | NewOctave         Octave
   | NewBaseOctave     BaseOctave
   | NewNotesPerOctave NotesPerOctave

   | NewChannel        Channel
   | NewVelocity       Velocity

   | PanicEvent
   | SaveConfig

     deriving (Show, Eq)


type EventHandlerBus    = MVar EventToHandle
type EventHandlerSender = EventToHandle → IO ()
type EventsListener     = EventToHandle → AppState → IO ()


-- We need this to trigger correct note-off when channel/base-key/base-pitch/channel is changed
-- before last triggered note is released.
data StoredEvent
   = StoredNoteOn Channel Pitch Velocity
     deriving (Show, Eq)


runEventHandler ∷ EventHandlerContext → IO EventHandlerInterface
runEventHandler ctx = do
  (bus ∷ EventHandlerBus) ← newEmptyMVar

  (appStateRef ∷ IORef AppState) ←
    let
      x = initialConfig ctx

      appState
        = AppState
        { baseKey        = baseKey        (x ∷ Config)
        , basePitch      = basePitch      (x ∷ Config)
        , octave         = octave         (x ∷ Config)
        , baseOctave     = baseOctave     (x ∷ Config)
        , notesPerOctave = notesPerOctave (x ∷ Config)
        , channel        = channel        (x ∷ Config)
        , velocity       = velocity       (x ∷ Config)

        , pitchMap       = getPitchMapping (baseKey (x ∷ Config)) (basePitch (x ∷ Config))
                                           (octave (x ∷ Config)) (baseOctave (x ∷ Config))
                                           (notesPerOctave (x ∷ Config))
        , storedEvents   = empty
        }
    in
      newIORef appState

  let interface
        = EventHandlerInterface
        { handleEvent = putMVar bus
        , getAppState = readIORef appStateRef
        }

      handle ∷ EventToHandle → IO ()

      handle (KeyPress k) = do
        appState ← readIORef appStateRef

        case lookup k $ pitchMap appState of

             Just p → do let ch     = channel  (appState ∷ AppState)
                             vel    = velocity (appState ∷ AppState)
                             stored = StoredNoteOn ch p vel

                         sendToMIDIPlayer ctx $ NoteOn ch p vel
                         updateState $ \s → s { storedEvents = insert k stored $ storedEvents s }

             Nothing → pure ()

      handle (KeyRelease k) = readIORef appStateRef >>= \appState → eitherValue $ do

        case lookup k $ storedEvents appState of

             Just (StoredNoteOn ch p vel) → Left $ do
               sendToMIDIPlayer ctx $ NoteOff ch p vel
               updateState $ \s → s { storedEvents = k `delete` storedEvents s }

             Nothing → Right ()

        pure $
          case lookup k $ pitchMap appState of
               Nothing → pure ()
               Just x  → sendToMIDIPlayer ctx
                       $ NoteOff (channel (appState ∷ AppState)) x $ velocity (appState ∷ AppState)

      handle (NewBaseKey k)        = updateState $ \s → s { baseKey        = k }
      handle (NewBasePitch p)      = updateState $ \s → s { basePitch      = p }
      handle (NewChannel c)        = updateState $ \s → s { channel        = c }
      handle (NewVelocity v)       = updateState $ \s → s { velocity       = v }
      handle (NewOctave o)         = updateState $ \s → s { octave         = o }
      handle (NewBaseOctave o)     = updateState $ \s → s { baseOctave     = o }
      handle (NewNotesPerOctave n) = updateState $ \s → s { notesPerOctave = n }

      handle PanicEvent = sendToMIDIPlayer ctx Panic

      handle SaveConfig = do
        s ← readIORef appStateRef

        saveConfig Config
          { configVersion  = def

          , baseKey        = baseKey        (s ∷ AppState)
          , basePitch      = basePitch      (s ∷ AppState)
          , octave         = octave         (s ∷ AppState)
          , baseOctave     = baseOctave     (s ∷ AppState)
          , notesPerOctave = notesPerOctave (s ∷ AppState)

          , channel        = channel        (s ∷ AppState)
          , velocity       = velocity       (s ∷ AppState)
          }

      updateState ∷ (AppState → AppState) → IO ()
      updateState (updateStateMiddleware → (• dupe) → f) =
        atomicModifyIORef' appStateRef f >>= onNewAppState ctx

  (interface <$) $ forkIO $ catchThreadFail [] "Event Handler" $ forever $ do
    let notifyListener ev = do
          !s ← readIORef appStateRef
          forkIO $ catchThreadFail [] "Events listener notifier" $ eventsListener ctx ev s

    takeMVar bus >>= (handle &&& notifyListener) • uncurry (>>)

  where updateStateMiddleware ∷ (AppState → AppState) → AppState → AppState
        updateStateMiddleware f oldState =

          if baseKey        (newState ∷ AppState) ≠ baseKey        (oldState ∷ AppState)
          || basePitch      (newState ∷ AppState) ≠ basePitch      (oldState ∷ AppState)
          || octave         (newState ∷ AppState) ≠ octave         (oldState ∷ AppState)
          || baseOctave     (newState ∷ AppState) ≠ baseOctave     (oldState ∷ AppState)
          || notesPerOctave (newState ∷ AppState) ≠ notesPerOctave (oldState ∷ AppState)
             then newState { pitchMap = getPitchMapping baseKey' basePitch'
                                                        octave' baseOctave'
                                                        notesPerOctave' }
             else newState

          where newState        = f oldState
                baseKey'        = baseKey        (newState ∷ AppState)
                basePitch'      = basePitch      (newState ∷ AppState)
                octave'         = octave         (newState ∷ AppState)
                baseOctave'     = baseOctave     (newState ∷ AppState)
                notesPerOctave' = notesPerOctave (newState ∷ AppState)


getPitchMapping ∷ RowKey → Pitch → Octave → BaseOctave → NotesPerOctave → HashMap RowKey Pitch
getPitchMapping baseKey' basePitch' octave' baseOctave' notesPerOctave' = result
  where result = uncurry union $ f $ splitAt splitKeysPos allKeysOrder

        f = first (reverse • drop leftSliceCount)
            >>> flip zip lp *** flip zip rp
            >>> fromList    *** fromList

        basePitchN      = fromPitch basePitch'
        octaveN         = fromIntegral $ fromOctave octave'
        baseOctaveN     = fromIntegral $ fromBaseOctave' baseOctave'
        notesPerOctaveN = fromIntegral $ fromNotesPerOctave notesPerOctave'

        minPitch        = fromPitch minBound
        maxPitch        = fromPitch maxBound

        octaveShift     = (baseOctaveN - octaveN) × notesPerOctaveN
        shiftedPitch    = basePitchN - octaveShift

        baseKeyPos      = fromMaybe (error [qm| Key {baseKey'} not found |])
                        $ baseKey' `elemIndex` allKeysOrder

        splitKeysPos    = baseKeyPos + abs (min 0 shiftedPitch)
        leftSliceCount  = max maxPitch (pred shiftedPitch) - maxPitch

        lp, rp ∷ [Pitch]
        rp = fmap toPitch [max minPitch shiftedPitch .. maxPitch]
        lp = fmap toPitch [x, pred x .. minPitch] where x = min maxPitch $ pred shiftedPitch


-- For extracting value from breakable monads
eitherValue ∷ Either α α → α
eitherValue (Left  x) = x
eitherValue (Right x) = x
