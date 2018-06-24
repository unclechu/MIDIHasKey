{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Types
     ( NotesPerOctave
     , fromNotesPerOctave
     , toNotesPerOctave

     , Octave
     , fromOctave
     , toOctave

     , BaseOctave
     , fromBaseOctave
     , fromBaseOctave'
     , toBaseOctave
     , toBaseOctave'

     , WindowTitle
     , AlertMessage (..)
     ) where

import Prelude.Unicode
import GHC.TypeLits

import Data.Word
import Text.InterpolatedString.QM
import Data.Text (type Text)

-- local
import Utils


type WindowTitle = "MIDIHasKey — Virtual MIDI keyboard for microtonal music"


data AlertMessage
   = ErrorAlert Text
   | InfoAlert  Text
     deriving (Eq, Show)


newtype NotesPerOctave = NotesPerOctave { fromNotesPerOctave ∷ Word8 } deriving (Eq, Show, Ord)

toNotesPerOctave ∷ Word8 → NotesPerOctave
toNotesPerOctave = NotesPerOctave • guardBounds

instance Bounded NotesPerOctave where
  minBound = NotesPerOctave 1
  maxBound = NotesPerOctave 128

instance Enum NotesPerOctave where
  succ x@(NotesPerOctave y) | x ≥ maxBound = error "Cannot `succ` `maxBound ∷ NotesPerOctave`"
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

toOctave ∷ Word8 → Octave
toOctave = Octave • guardBounds

instance Bounded Octave where
  minBound = Octave 1
  maxBound = Octave 16

instance Enum Octave where
  succ x@(Octave y) | x ≥ maxBound = error "Cannot `succ` `maxBound ∷ Octave`"
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

toBaseOctave ∷ Octave → BaseOctave
toBaseOctave = BaseOctave
{-# INLINE toBaseOctave #-}

toBaseOctave' ∷ Word8 → BaseOctave
toBaseOctave' = toBaseOctave ∘ toOctave
{-# INLINE toBaseOctave' #-}

fromBaseOctave' ∷ BaseOctave → Word8
fromBaseOctave' = fromBaseOctave • fromOctave
{-# INLINE fromBaseOctave' #-}


guardBounds ∷ ∀ α. (Ord α, Show α, Bounded α) ⇒ α → α
guardBounds x | x < minBound ∨ x > maxBound = error [qms| {x} is out of bounds,
                                                          it supposed to be between
                                                          {minBound ∷ α} and {maxBound ∷ α} |]
              | otherwise = x
