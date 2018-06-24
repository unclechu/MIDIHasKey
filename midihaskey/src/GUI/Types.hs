{-# LANGUAGE UnicodeSyntax #-}

module GUI.Types
     ( GUIContext (..)
     , GUIState (..)
     , GUIInterface (..)
     , GUIStateUpdate (..)
     ) where

import Prelude
import Prelude.Unicode

import Data.HashMap.Strict

import Sound.MIDI.Message.Channel

-- local
import Types
import MIDIHasKey.Utils
import Keys.Types
import Keys.Specific.GUI


data GUIContext
   = GUIContext
   { initialState             ∷ GUIState

   , appExitHandler           ∷ IO ()
   , panicButtonHandler       ∷ IO ()
   , saveConfigButtonHandler  ∷ IO ()

   , setBaseKeyHandler        ∷ RowKey → IO ()
   , setBasePitchHandler      ∷ Pitch → IO ()
   , setOctaveHandler         ∷ Octave → IO ()
   , setBaseOctaveHandler     ∷ BaseOctave → IO ()
   , setNotesPerOctaveHandler ∷ NotesPerOctave → IO ()

   , selectChannelHandler     ∷ Channel → IO ()

   , noteButtonHandler        ∷ RowKey → 𝔹 → IO ()
   }

data GUIState
   = GUIState
   { guiStateBaseKey        ∷ RowKey
   , guiStateBasePitch      ∷ Pitch
   , guiStateOctave         ∷ Octave
   , guiStateBaseOctave     ∷ BaseOctave
   , guiStateNotesPerOctave ∷ NotesPerOctave

   , guiStatePitchMapping   ∷ HashMap RowKey Pitch

   , guiStateChannel        ∷ Channel
   , guiStateVelocity       ∷ Velocity
   } deriving (Eq, Show)

data GUIInterface
   = GUIInterface
   { guiStateUpdate ∷ GUIStateUpdate → IO ()
   , guiShowAlert   ∷ AlertMessage → IO ()
   }

data GUIStateUpdate
   = SetBaseKey        RowKey
   | SetBasePitch      Pitch
   | SetOctave         Octave
   | SetBaseOctave     BaseOctave
   | SetNotesPerOctave NotesPerOctave

   | SetPitchMapping   (HashMap RowKey Pitch)

   | SetChannel        Channel
   | SetVelocity       Velocity

   | KeyButtonState    RowKey 𝔹

   | NewLastSavedState GUIState

     deriving (Show, Eq)
