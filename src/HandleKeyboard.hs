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
import Control.Concurrent (forkIO)

import System.Linux.Input.Event
import System.IO (Handle, IOMode (ReadMode), openBinaryFile)

-- local
import Utils
import Keys.Types
import Keys.Specific.HandleKeyboard
import EventHandler


type DeviceReader = IO (RowKey, Bool)

data HandleKeyboardContext
  = HandleKeyboardContext
  { devices                ∷ [FilePath]
  , handleKeyboardKeyEvent ∷ RowKey → Bool → IO ()
  }


getDeviceReaders ∷ [FilePath] → IO [DeviceReader]
getDeviceReaders devicesPaths =
  mapM (`openBinaryFile` ReadMode) devicesPaths <&> map (hReadEvent • parseEvent)

  where parseEvent deviceReader = deviceReader >>= \case

          Just KeyEvent
            { evKeyEventType = (filterKeyPressRelease → Just keyState)
            , evKeyCode      = (flip lookup allKeys   → Just key)
            } → pure (key, keyState)

          _ → parseEvent deviceReader

        filterKeyPressRelease = \case
          Depressed → Just True
          Released  → Just False
          _         → Nothing


runKeyboardHandling ∷ HandleKeyboardContext → IO ()
runKeyboardHandling ctx =
  getDeviceReaders devices' <&> zip devices' >>= mapM_ (uncurry runReader)

  where devices' = devices ctx
        readerThreadName x = "Keyboard events reader for '" ⧺ x ⧺ "'"

        runReader ∷ FilePath → DeviceReader → IO ()
        runReader devPath devReader =
          void $ forkIO $ catchThreadFail (readerThreadName devPath) $ forever $
            devReader >>= uncurry (handleKeyboardKeyEvent ctx)
