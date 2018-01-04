{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}

-- Type-level
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils where

import Prelude.Unicode
import GHC.TypeLits
import Data.Word
import Data.Proxy
import Data.Kind


data (a ∷ k) ↔ b
infixr 5 ↔

-- How many elements combined with (↔)
type family Len (a ∷ k) ∷ Nat where
  Len (x ↔ xs) = Len x + Len xs
  Len _ = 1

type family KnownSymbols (a ∷ [Symbol]) ∷ Constraint where
  KnownSymbols '[] = ()
  KnownSymbols (s ': t) = (KnownSymbol s, KnownSymbols t)


-- Key-type to MIDI key
nat2MidiKey ∷ (KnownNat a) ⇒ Proxy a → Word8
nat2MidiKey = fromInteger ∘ natVal

superscript ∷ Char → Char
superscript = \case '1' → '¹' ; '2' → '²' ; '3' → '³' ; '4' → '⁴' ; '5' → '⁵'
                    '6' → '⁶' ; '7' → '⁷' ; '8' → '⁸' ; '9' → '⁹' ; '0' → '⁰'

-- Left-to-right composition, just like (>=>) for monads.
(•) ∷ (a → b) → (b → c) → (a → c)
(•) = flip (∘)
{-# INLINE (•) #-}
infixl 9 •
