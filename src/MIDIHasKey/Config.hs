{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitNamespaces #-}

module MIDIHasKey.Config
     ( Config (..)
     , parseConfig
     , readConfig
     , saveConfig
     ) where

import Prelude hiding (readFile, writeFile)
import Prelude.Unicode
import GHC.Generics

import Data.Default
import Data.ByteString hiding (pack, unpack, break, writeFile)
import Data.ByteString.Lazy (writeFile)
import Data.String (type IsString, fromString)
import Data.Aeson
import Data.Aeson.Types
import Data.Text (type Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Text.InterpolatedString.QM
import Data.Scientific (toRealFloat)
import qualified Data.HashMap.Strict as HM
import Data.Attoparsec.Text

import Control.Monad (unless)

import Sound.MIDI.Message.Channel
import Sound.MIDI.Message.Channel.Voice (normalVelocity)

import System.Directory
import System.FilePath

-- local
import Utils
import Types
import Keys.Types


-- "Major" breaks backward compatibility
-- "Minor" increases when something new is added
--         but config still could be used by older version of application
data ConfigVersion = ConfigVersion Word Word deriving (Show, Eq, Ord)
instance Default ConfigVersion where def = ConfigVersion 1 0 -- Current version of config

showConfigVersionAsNumber ∷ (Monoid α, IsString α) ⇒ ConfigVersion → α
showConfigVersionAsNumber (ConfigVersion major minor) = [qm| {major}.{minor} |]

instance FromJSON ConfigVersion where
  parseJSON x@(String str) =
    case parser `parseOnly` str of
         Right a → pure a
         Left  _ → fail [qms| ConfigVersion supposed to be represented as "N.N" (string)
                              where "N" is a decimal unsigned number, got this: "{str}" |]

    where parser = ConfigVersion <$> decimal <* char '.' <*> decimal

  parseJSON x = typeMismatch "ConfigVersion" x

instance ToJSON ConfigVersion where
  toJSON = String ∘ showConfigVersionAsNumber


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

instance Default Config where
  def
    = Config
    { configVersion  = def

    , baseKey        = AKey
    , basePitch      = toPitch 19 -- 20th in [1..128]
    , octave         = fromBaseOctave baseOctave'
    , baseOctave     = baseOctave'
    , notesPerOctave = toNotesPerOctave 12

    , channel        = minBound
    , velocity       = normalVelocity
    }
    where
      baseOctave' = toBaseOctave' 4


parseConfig ∷ ByteString → Either String Config
parseConfig src = do
  -- Raw JSON value to parse just config version
  (jsonConfig ∷ Value) <- eitherDecodeStrict' src

  -- Parsing only config version from raw JSON, if config version is incompatible parsing of whole
  -- structure may fail but we need to show proper fail message about incompatible config version so
  -- that's why we parsing raw JSON first.
  parsedConfVer@(ConfigVersion parsedMajor _) <-
    case jsonConfig of
         (Object (HM.lookup "configVersion" → Just x@(String _))) →
           case fromJSON x of
                Success x → Right x
                Error msg → Left [qms| Parsing config version is failed with message: {msg} |]
         _ → Left "Parsing config version is failed"

  -- If major version of config is bigger than default version from current program it means we have
  -- breaking changes and it could not be read by current version of the MIDIHasKey.
  if implMajor < parsedMajor
     then Left [qms| Version of config {showConfigVersionAsNumber parsedConfVer ∷ Text} is
                     incompatible with currently implemented
                     {showConfigVersionAsNumber implementedConfVer ∷ Text} |]

     else -- Now we're parsing real `Config` after we ensured that version of config is compatible.
          -- Parsing `Config` from already parsed `Value` instead of just parsing again from source
          -- `ByteString`.
          case fromJSON jsonConfig of
               Success x → Right x
               Error msg → Left msg

  where implementedConfVer@(ConfigVersion implMajor _) = def


readConfig ∷ IO (Either String Config)
readConfig = do
  cfgFile ← getDefaultConfigFilePath
  doesConfigExist ← doesFileExist cfgFile

  -- If config file doesn't exist just returning default config
  if doesConfigExist
     then readFile cfgFile <&> parseConfig
     else pure $ Right def


saveConfig ∷ Config → IO ()
saveConfig cfg = do
  cfgFile ← getDefaultConfigFilePath
  writeFile cfgFile $ encode cfg


getDefaultConfigFilePath ∷ IO FilePath
getDefaultConfigFilePath = do
  -- Getting application config directory.
  -- It creates new if it not exists.
  -- E.g. ~/.config/MIDIHasKey
  !dir ← do
    dir ← getXdgDirectory XdgConfig appName
    doesExist ← doesDirectoryExist dir
    dir <$ unless doesExist (createDirectory dir)

  -- Config file would be named as `config.MIDIHasKey`
  pure $ dir </> "config" <.> appName

  where appName = "MIDIHasKey"
