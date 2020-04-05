{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

-- Type-level
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE ForeignFunctionInterface #-}

module MIDIHasKey.Utils
     ( (•), (<&!>), (⋄), type (↔), type Len, type 𝔹
     , module Data.Function
     , module Data.Functor
     , CatchThreadFailFlag (..)
     , catchThreadFail
     , dupe
     , maybeMUnit
     , maybeMUnit'
     , exit
     , nat2MidiKey
     ) where

import Prelude.Unicode
import GHC.TypeLits
import Data.Proxy
import Data.Word
import Data.Maybe (maybe)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Monoid
import Text.InterpolatedString.QM

import Control.Monad ((<$!>), void)
import Control.Exception (SomeException, BlockedIndefinitelyOnMVar, handle)

import Sound.MIDI.Message.Channel
import System.IO (hPutStrLn, hPrint, stderr)

foreign import ccall "exit" exit ∷ IO ()


data (α ∷ κ) ↔ β
infixr 5 ↔

-- How many elements combined with (↔)
type family Len (α ∷ κ) ∷ Nat where
  Len (x ↔ xs) = Len x + Len xs
  Len _ = 1

type 𝔹 = Bool


-- Key-type to MIDI key
nat2MidiKey ∷ KnownNat α ⇒ Proxy α → Pitch
nat2MidiKey = toPitch ∘ fromInteger ∘ natVal


-- Left-to-right composition, just like (>=>) for monads.
(•) ∷ (α → β) → (β → γ) → (α → γ)
(•) = flip (∘)
{-# INLINE (•) #-}
infixl 9 •

-- Left-to-right infix strict fmap
(<&!>) ∷ Monad μ ⇒ μ α → (α → β) → μ β
(<&!>) = flip (<$!>)
{-# INLINE (<&!>) #-}
infixr 5 <&!>

-- Generic concatenation
(⋄) ∷ Monoid α ⇒ α → α → α
(⋄) = (<>)
{-# INLINE (⋄) #-}
infixr 6 ⋄


data CatchThreadFailFlag
   = MVarInfLockIsOkay
     deriving (Eq, Show)

-- Helps to prevent undefined behavior when application still working after some of its subsystem is
-- failed. Usually it goes okay, but if something unexpectedly goes wrong, we shouldn't continue
-- working and making user to be confused.
catchThreadFail ∷ [CatchThreadFailFlag] → String → IO () → IO ()
catchThreadFail flags threadName
  = (if MVarInfLockIsOkay `elem` flags then handle mVarInfLockHandler else id)
  • handle etcHandler
  where
    mVarInfLockHandler (e ∷ BlockedIndefinitelyOnMVar) =
      hPutStrLn stderr [qms| "{threadName}" is stopped by "{e}" exception,
                             we're taking it as okay, because it is probably a listener
                             which doesn't have enough calls yet but designed properly
                             for further implementations. |]

    etcHandler (e ∷ SomeException) = do
      hPutStrLn stderr [qm| "{threadName}" thread has failed! |]
      hPrint stderr e
      exit


dupe ∷ α → (α, α)
dupe x = (x, x)
{-# INLINE dupe #-}

maybeMUnit ∷ Monad μ ⇒ (α → μ β) → Maybe α → μ ()
maybeMUnit f = maybe (pure ()) (void ∘ f)
{-# INLINE maybeMUnit #-}

-- With flipped arguments
maybeMUnit' ∷ Monad μ ⇒ Maybe α → (α → μ β) → μ ()
maybeMUnit' x f = maybe (pure ()) (void ∘ f) x
{-# INLINE maybeMUnit' #-}
