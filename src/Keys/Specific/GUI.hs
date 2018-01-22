{-# LANGUAGE UnicodeSyntax #-}

-- Type-level
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Keys.Specific.GUI
     ( GUIKeyOfRow
     , GUIKeysRow
     , GetAllGUIRows (..)
     ) where

import Prelude.Unicode
import GHC.TypeLits

import Data.Proxy
import Data.Singletons.TH

import Sound.MIDI.Message.Channel

-- local
import Keys.Types
import Keys.Helpers


type GUIKeyOfRow = (RowKey, String, Pitch)
type GUIKeysRow  = [GUIKeyOfRow]


class GetAllGUIRows a where
  getAllGUIRows ∷ Pitch → Proxy a → [GUIKeysRow]
instance GetAllGUIRows '[] where
  getAllGUIRows _ Proxy = []
instance (GetAllGUIRows t, KnownList l, KeyRow l, KnownNat n, l ~ RowList r, n ~ RowOffset r)
  ⇒ GetAllGUIRows (r ': t) where
  getAllGUIRows n Proxy = getRow n proxies : getAllGUIRows n (Proxy ∷ Proxy t)

    where proxies = (Proxy ∷ Proxy l, Proxy ∷ Proxy n)

          getRow ∷ (KeyRow l, KnownNat o) ⇒ Pitch → (Proxy l, Proxy o) → GUIKeysRow
          getRow n (listProxy, offsetProxy) = mappedRow [startFrom..] listProxy
            where startFrom = toPitch $ fromPitch n + fromInteger (natVal offsetProxy)


-- Internal helper
class KeyRow (a ∷ [(RowKey, Symbol, Nat)]) where
  mappedRow ∷ [Pitch] -- ^ List of pitches instead of start pitch for future flexibility
            → Proxy a
            → GUIKeysRow

instance KeyRow '[] where
  mappedRow _ Proxy = []
instance (SingI k, KnownSymbol s, KeyRow t, KnownNat m) ⇒ KeyRow ('(k, s, m) ': t) where
  mappedRow (pitch : ps) Proxy
    = (fromSing (sing ∷ Sing k), symbolVal (Proxy ∷ Proxy s), pitch)
    : mappedRow ps (Proxy ∷ Proxy t)
