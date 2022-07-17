{-# LANGUAGE UnicodeSyntax #-}

-- Type-level
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Keys.Helpers where

import GHC.TypeLits

-- local
import MIDIHasKey.Utils (Len)
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
