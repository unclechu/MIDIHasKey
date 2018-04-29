{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Types where

import Prelude.Unicode
import GHC.TypeLits

import Data.Word
import Text.InterpolatedString.QM

-- local
import Utils


type WindowTitle = "MIDIHasKey — Virtual MIDI keyboard for microtonal music"


newtype NotesPerOctave = NotesPerOctave { fromNotesPerOctave ∷ Word8 } deriving (Eq, Show, Ord)

instance Bounded NotesPerOctave where
  minBound = NotesPerOctave 1
  maxBound = NotesPerOctave 128

instance Enum NotesPerOctave where
  succ x@(NotesPerOctave y) | x ≥ maxBound = error "Cannot `succ` `minBound ∷ NotesPerOctave`"
                            | otherwise = NotesPerOctave $ succ y

  pred x@(NotesPerOctave y) | x ≤ minBound = error "Cannot `pred` `minBound ∷ NotesPerOctave`"
                            | otherwise = NotesPerOctave $ pred y

  toEnum n | n < min ∨ n > max = error [qm| Cannot `toEnum` {n} to `NotesPerOctave` |]
           | otherwise = NotesPerOctave $ toEnum n

    where min = fromIntegral $ fromNotesPerOctave minBound
          max = fromIntegral $ fromNotesPerOctave maxBound

  fromEnum (NotesPerOctave x) = fromEnum x

  enumFrom     x   = enumFromTo     x maxBound
  enumFromThen x y = enumFromThenTo x y bound
    where bound | fromEnum y ≥ fromEnum x = maxBound
                | otherwise               = minBound


newtype Octave = Octave { fromOctave ∷ Word8 } deriving (Eq, Show, Ord)

instance Bounded Octave where
  minBound = Octave 1
  maxBound = Octave 16

instance Enum Octave where
  succ x@(Octave y) | x ≥ maxBound = error "Cannot `succ` `minBound ∷ Octave`"
                    | otherwise = Octave $ succ y

  pred x@(Octave y) | x ≤ minBound = error "Cannot `pred` `minBound ∷ Octave`"
                    | otherwise = Octave $ pred y

  toEnum n | n < min ∨ n > max = error [qm| Cannot `toEnum` {n} to `Octave` |]
           | otherwise = Octave $ toEnum n

    where min = fromIntegral $ fromOctave minBound
          max = fromIntegral $ fromOctave maxBound

  fromEnum (Octave x) = fromEnum x

  enumFrom     x   = enumFromTo     x maxBound
  enumFromThen x y = enumFromThenTo x y bound
    where bound | fromEnum y ≥ fromEnum x = maxBound
                | otherwise               = minBound

-- To avoid human-factor errors
newtype BaseOctave = BaseOctave { fromBaseOctave ∷ Octave } deriving (Eq, Show, Ord)
