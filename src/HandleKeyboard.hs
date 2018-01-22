{-# LANGUAGE UnicodeSyntax #-}

module HandleKeyboard where

import Prelude.Unicode

import System.Linux.Input.Event

-- local
import Utils

data HandleKeyboardContext
  = HandleKeyboardContext
  {
  }


runKeyboardHandling ∷ IO ()
runKeyboardHandling = do
  putStrLn "running keyboard handling"
