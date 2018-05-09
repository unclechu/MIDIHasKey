{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module MIDIHasKey.Config
     ( parseConfig
     ) where

import Prelude
import Prelude.Unicode
import GHC.Generics

import Data.Default
import Data.ByteString.Lazy hiding (pack, unpack, break)
import Data.String (fromString)
import Data.Aeson
import Data.Aeson.Types
import Data.Text (pack, unpack)
import Text.InterpolatedString.QM
import Data.Scientific (toRealFloat)

import Sound.MIDI.Message.Channel

-- local
import Utils
import Types
import Keys.Types


data ConfigVersion = ConfigVersion Word Word deriving (Show, Eq, Ord)
instance Default ConfigVersion where def = ConfigVersion 1 0

instance FromJSON ConfigVersion where
  parseJSON (String str) = parseVer $ break (≡ '.') $ unpack str
    where parseVer (a, ('.' : b)) = pure $ ConfigVersion (read a) (read b)
          parseVer (_, b) =
            error [qm| Unexpected value of minor version of ConfigVersion: "{b}" |]

  parseJSON x = typeMismatch "ConfigVersion" x

instance ToJSON ConfigVersion where
  toJSON (ConfigVersion major minor) = String [qm| {major}.{minor} |]


instance FromJSON RowKey where
  parseJSON (String str) = pure $ read $ unpack str
  parseJSON x = typeMismatch "RowKey" x

instance ToJSON RowKey where
  toJSON = String ∘ pack ∘ show


-- In JSON it must be represented as [1..128] while `Pitch` is actually [0..127]
instance FromJSON Pitch where
  parseJSON x@(Number (toRealFloat → properFraction → (pred → n, r)))
    | r ≠ 0 ∨ n < fromPitch minBound ∨ n > fromPitch maxBound = typeMismatch "Pitch" x
    | otherwise = pure $ toPitch n

  parseJSON x = typeMismatch "Pitch" x

-- In JSON it must be represented as [1..128] while `Pitch` is actually [0..127]
instance ToJSON Pitch where
  toJSON (fromPitch → succ → fromIntegral → n) = Number n


instance FromJSON Octave where
  parseJSON x@(Number (toRealFloat → properFraction → (n, r)))
    | r ≠ 0 ∨ n < fromOctave minBound ∨ n > fromOctave maxBound = typeMismatch "Octave" x
    | otherwise = pure $ toOctave n

  parseJSON x = typeMismatch "Octave" x

instance ToJSON Octave where
  toJSON (fromOctave → fromIntegral → n) = Number n


instance FromJSON BaseOctave where
  parseJSON x@(Number (toRealFloat → properFraction → (n, r)))
    | r ≠ 0 ∨ n < fromOctave minBound ∨ n > fromOctave maxBound = typeMismatch "BaseOctave" x
    | otherwise = pure $ toBaseOctave' n

  parseJSON x = typeMismatch "BaseOctave" x

instance ToJSON BaseOctave where
  toJSON (fromBaseOctave' → fromIntegral → n) = Number n


instance FromJSON NotesPerOctave where
  parseJSON x@(Number (toRealFloat → properFraction → (n, r)))
    | r ≠ 0 ∨ n < fromNotesPerOctave minBound ∨ n > fromNotesPerOctave maxBound =
        typeMismatch "NotesPerOctave" x

    | otherwise = pure $ toNotesPerOctave n

  parseJSON x = typeMismatch "NotesPerOctave" x

instance ToJSON NotesPerOctave where
  toJSON (fromNotesPerOctave → fromIntegral → n) = Number n


-- In JSON it must be represented as [1..16] while `Channel` is actually [0..15]
instance FromJSON Channel where
  parseJSON x@(Number (toRealFloat → properFraction → (pred → n, r)))
    | r ≠ 0 ∨ n < fromChannel minBound ∨ n > fromChannel maxBound = typeMismatch "Channel" x
    | otherwise = pure $ toChannel n

  parseJSON x = typeMismatch "Channel" x

-- In JSON it must be represented as [1..16] while `Channel` is actually [0..15]
instance ToJSON Channel where
  toJSON (fromChannel → succ → fromIntegral → n) = Number n


-- In JSON it must be represented as [1..128] while `Velocity` is actually [0..127]
instance FromJSON Velocity where
  parseJSON x@(Number (toRealFloat → properFraction → (pred → n, r)))
    | r ≠ 0 ∨ n < fromVelocity minBound ∨ n > fromVelocity maxBound = typeMismatch "Velocity" x
    | otherwise = pure $ toVelocity n

  parseJSON x = typeMismatch "Velocity" x

-- In JSON it must be represented as [1..128] while `Velocity` is actually [0..127]
instance ToJSON Velocity where
  toJSON (fromVelocity → succ → fromIntegral → n) = Number n


data Config
  = Config
  { configVersion  ∷ ConfigVersion

  , baseKey        ∷ RowKey
  , basePitch      ∷ Pitch
  , octave         ∷ Octave
  , baseOctave     ∷ BaseOctave
  , notesPerOctave ∷ NotesPerOctave

  , channel        ∷ Channel
  , velocity       ∷ Velocity
  } deriving (Show, Eq, Generic)

instance FromJSON Config
instance ToJSON Config


parseConfig ∷ ByteString → Either String Config
parseConfig = undefined
