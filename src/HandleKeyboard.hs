{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module HandleKeyboard
     ( HandleKeyboardContext (..)
     , runKeyboardHandling
     ) where

import Prelude hiding (lookup)
import Prelude.Unicode

import Data.HashMap.Strict hiding (map)

import Control.Monad

import System.Linux.Input.Event
import System.IO (Handle, IOMode (ReadMode), openBinaryFile)

-- local
import Utils
import Keys.Types
import Keys.Specific.HandleKeyboard


type DeviceReader = IO (Bool, RowKey)

data HandleKeyboardContext
  = HandleKeyboardContext
  { devices ∷ [FilePath]
  }


getDeviceReaders ∷ [FilePath] → IO [DeviceReader]
getDeviceReaders devicesPaths =
  mapM (`openBinaryFile` ReadMode) devicesPaths <&> map (hReadEvent • parseEvent)

  where parseEvent deviceReader = deviceReader >>= \case

          Just KeyEvent
            { evKeyEventType = (filterKeyPressRelease → Just keyState)
            , evKeyCode      = (flip lookup allKeys   → Just key)
            } → pure (keyState, key)

          _ → parseEvent deviceReader

        filterKeyPressRelease = \case
          Depressed → Just True
          Released  → Just False
          _         → Nothing


runKeyboardHandling ∷ HandleKeyboardContext → IO ()
runKeyboardHandling ctx = do
  eventReaders ← getDeviceReaders $ devices ctx
  pure ()
