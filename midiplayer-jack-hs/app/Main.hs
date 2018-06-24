-- TODO this is only prototype, not implemented yet

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where -- "midiplayer-jack-hs" app

import           Prelude.Unicode
import qualified Foreign.C.Error as Foreign

import           Data.Proxy
import           Data.IORef
import           Data.Maybe
import           Data.Primitive.Ptr (Ptr, nullPtr)

import           Control.Monad
import           Control.Monad.Exception.Synchronous ( Exceptional (Success, Exception)
                                                     , runExceptionalT
                                                     )

import           System.Exit
import           System.IO (hPutStrLn, stderr)

import           Sound.JACK hiding (Port)
import           Sound.JACK.MIDI hiding (main)
import           Sound.JACK.Exception (Status, Errno (Errno), PortRegister)

import           MIDIHasKey.Utils


-- TODO read raw MIDI events from stdin
main ∷ IO ()
main = initJACK >> waitForBreak


-- TODO implement
jackProcess ∷ Port Output -> NFrames -> IO ()
jackProcess _ _ = pure ()


-- Returns JACK client deactivator monad
initJACK ∷ IO (IO ())
initJACK = do
  !jackClient ←
    runExceptionalT (newClientDefault clientName) >>=
      handleException (Proxy ∷ Proxy (Status ()))
        "Failed to initialize JACK client"
        Nothing

  !(outPort ∷ Port Output) ←
    handleException (Proxy ∷ Proxy (PortRegister ()))
      "Failed to register JACK MIDI port" Nothing =<<
        runExceptionalT (newPort jackClient outputPortName)

  processPtr ←
    let process = jackProcess outPort
     in makeProcess $ \nframes _ → Foreign.eOK <$ process nframes

  runExceptionalT (setProcess jackClient processPtr nullPtr) >>=
    handleException (Proxy ∷ Proxy (Errno ()))
      "Failed to set JACK process callback"
      (Just errnoExceptionReport)

  runExceptionalT (activate jackClient) >>=
    handleException (Proxy ∷ Proxy (Errno ()))
      "Failed to activate JACK client"
      (Just errnoExceptionReport)

  pure $
    runExceptionalT (deactivate jackClient) >>=
      handleException (Proxy ∷ Proxy (Errno ()))
        "Failed to deactivate JACK client"
        (Just errnoExceptionReport)


handleException
  ∷ Proxy ε
  → String
  → Maybe (String → ε → String)
  → Exceptional ε α
  → IO α
handleException Proxy _ _ (Success   x) = pure x
handleException Proxy m f (Exception e) = do
  hPutStrLn stderr $ fromMaybe m $ f <*> pure m <*> pure e
  exitWith $ ExitFailure 1


errnoExceptionReport ∷ String → Errno α → String
errnoExceptionReport m (Errno (Foreign.Errno x)) = m ⋄ ": Errno " ⋄ show x
errnoExceptionReport m _ = m


clientName, outputPortName ∷ String
clientName     = "MIDIHasKey"
outputPortName = "output"
