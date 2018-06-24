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
     ( allGUIRows
     , GUIKeyOfRow
     , GUIKeysRow
     ) where

import Prelude.Unicode
import GHC.TypeLits

import Data.Proxy
import Data.Singletons.TH

import Sound.MIDI.Message.Channel

-- local
import Keys.Types
import Keys.Helpers
import Utils (type (↔), (⋄))


type GUIKeyOfRow = (RowKey, String)
type GUIKeysRow  = [GUIKeyOfRow]


class AllRows a where
  getAllRows ∷ Proxy a → [GUIKeysRow]

instance AllRows '[] where
  getAllRows Proxy = []

instance (RowKeys h, AllRows t) ⇒ AllRows (h ': t) where
  getAllRows Proxy = getRowKeys (Proxy ∷ Proxy h) : getAllRows (Proxy ∷ Proxy t)


class RowKeys a where
  getRowKeys ∷ Proxy a → GUIKeysRow

instance (RowKeys h, RowKeys t) ⇒ RowKeys (h ↔ t) where
  getRowKeys Proxy = getRowKeys (Proxy ∷ Proxy h) ⋄ getRowKeys (Proxy ∷ Proxy t)

instance (SingI x, s ~ GetLabel x, KnownSymbol s) ⇒ RowKeys (x ∷ RowKey) where
  getRowKeys Proxy = [(fromSing (sing ∷ Sing x), symbolVal (Proxy ∷ Proxy s))]


allGUIRows ∷ [GUIKeysRow]
allGUIRows = getAllRows (Proxy ∷ Proxy AllKeysRows)
