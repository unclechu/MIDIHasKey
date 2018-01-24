{-# LANGUAGE UnicodeSyntax #-}

-- Type-level
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Keys.Helpers where

import Prelude.Unicode
import GHC.TypeLits

import Data.Kind (Constraint)
import Data.Singletons.TH (SingI)

-- local
import Utils (type (↔), Len)
import Keys.Types


-- Type-level offsets of keys rows (offset of ordered key numbers)
type family RowOffset (a ∷ k) ∷ Nat where
  RowOffset KeysRow1 = 0
  RowOffset KeysRow2 = Len KeysRow1
  RowOffset KeysRow3 = Len KeysRow1 + Len KeysRow2
  RowOffset KeysRow4 = Len KeysRow1 + Len KeysRow2 + Len KeysRow3


type family GetLabel (a ∷ RowKey) ∷ Symbol where
  GetLabel key = LabelByRowKey RowKeyMap key
-- A helper for `GetLabel`
type family LabelByRowKey (l ∷ [(RowKey, Symbol, Nat)]) (k ∷ RowKey) ∷ Symbol where
  LabelByRowKey ('(k, label, _) ': _) k = label
  LabelByRowKey (_ ': t) k = LabelByRowKey t k

type family GetEvDevKey (a ∷ RowKey) ∷ Nat where
  GetEvDevKey key = EvDevKeyByRowKey RowKeyMap key
-- A helper for `GetEvDevKey`
type family EvDevKeyByRowKey (l ∷ [(RowKey, Symbol, Nat)]) (k ∷ RowKey) ∷ Nat where
  EvDevKeyByRowKey ('(k, _, evDevKey) ': _) k = evDevKey
  EvDevKeyByRowKey (_ ': t) k = EvDevKeyByRowKey t k


-- A constraint for `Known*` type-level values of `RowKeyMap`
type family KnownList (a ∷ [(RowKey, Symbol, Nat)]) ∷ Constraint where
  KnownList '[] = ()
  KnownList (h ': t) = (KnownItem h, KnownList t)

type family KnownItem (a ∷ (RowKey, Symbol, Nat)) ∷ Constraint where
  KnownItem '(r, s, n) = (SingI r, KnownSymbol s, KnownNat n)

-- Build type-level list from keys row glued with (↔) combinator
type family RowList (a ∷ k) ∷ [(RowKey, Symbol, Nat)] where
  RowList (x ↔ xs) = '(x, GetLabel x, GetEvDevKey x) ': RowList xs
  RowList x = '(x, GetLabel x, GetEvDevKey x) ': '[]
