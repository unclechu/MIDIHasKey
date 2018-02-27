{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Type-level
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE ForeignFunctionInterface #-}

module Utils where

import Prelude.Unicode
import GHC.TypeLits
import Data.Proxy
import Data.Word
import qualified Data.Function ((&))

import Control.Monad ((<$!>))
import Control.Exception (SomeException, handle)

import Sound.MIDI.Message.Channel
import System.IO (hPutStrLn, hPrint, stderr)

foreign import ccall "exit" exit ∷ IO ()


data (a ∷ k) ↔ b
infixr 5 ↔

-- How many elements combined with (↔)
type family Len (a ∷ k) ∷ Nat where
  Len (x ↔ xs) = Len x + Len xs
  Len _ = 1


-- Key-type to MIDI key
nat2MidiKey ∷ (KnownNat a) ⇒ Proxy a → Pitch
nat2MidiKey = toPitch ∘ fromInteger ∘ natVal

superscript ∷ Char → Char
superscript = \case '1' → '¹' ; '2' → '²' ; '3' → '³' ; '4' → '⁴' ; '5' → '⁵'
                    '6' → '⁶' ; '7' → '⁷' ; '8' → '⁸' ; '9' → '⁹' ; '0' → '⁰'
                    x → x


-- Left-to-right composition, just like (>=>) for monads.
(•) ∷ (a → b) → (b → c) → (a → c)
(•) = flip (∘)
{-# INLINE (•) #-}
infixl 9 •

(&) ∷ a → (a → b) → b
(&) = (Data.Function.&)
{-# INLINE (&) #-}
infixl 1 &

-- Left-to-right infix fmap
-- Look at https://github.com/ekmett/lens/blob/d561c44098a1131dc26e545f6bfde58874bf6a6c/src/Control/Lens/Lens.hs#L357-L364
(<&>) :: Functor f ⇒ f a → (a → b) → f b
(<&>) = flip (<$>)
{-# INLINE (<&>) #-}
infixr 5 <&>

-- Left-to-right infix strict fmap
(<&!>) :: Monad m ⇒ m a → (a → b) → m b
(<&!>) = flip (<$!>)
{-# INLINE (<&!>) #-}
infixr 5 <&!>


-- Helps to prevent undefined behavior when application still working after some of its subsystem is
-- failed. Usually it goes okay, but if something unexpectedly goes wrong, we shouldn't continue
-- working and making user to be confused.
catchThreadFail ∷ String → IO () → IO ()
catchThreadFail (("'" ⧺) → (⧺ "' thread is failed!") → failMsg) =
  handle $ \(e ∷ SomeException) → hPutStrLn stderr failMsg >> hPrint stderr e >> exit

dupe ∷ a → (a, a)
dupe x = (x, x)
{-# INLINE dupe #-}
