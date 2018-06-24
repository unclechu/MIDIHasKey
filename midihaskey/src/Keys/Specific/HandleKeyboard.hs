{-# LANGUAGE UnicodeSyntax #-}

-- Type-level
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Keys.Specific.HandleKeyboard
     ( allKeys
     ) where

import Prelude.Unicode
import GHC.TypeLits

import Data.Proxy
import Data.Singletons.TH
import Data.Hashable
import Data.HashMap.Strict
import Data.Word

import System.Linux.Input.Event.Constants (Key (Key))

-- local
import Keys.Types
import Keys.Helpers
import MIDIHasKey.Utils (type (↔))


instance Hashable Key where
  hashWithSalt s (Key x) = s `hashWithSalt` x


class AllKeys a where
  getAllKeys ∷ Proxy a → HashMap Key RowKey

instance AllKeys '[] where
  getAllKeys Proxy = empty

-- List of all rows
instance (AllKeys h, AllKeys t) ⇒ AllKeys (h ': t) where
  getAllKeys Proxy = getAllKeys (Proxy ∷ Proxy h) `union` getAllKeys (Proxy ∷ Proxy t)

-- Keys of a row
instance (AllKeys h, AllKeys t) ⇒ AllKeys (h ↔ t) where
  getAllKeys Proxy = getAllKeys (Proxy ∷ Proxy h) `union` getAllKeys (Proxy ∷ Proxy t)

-- A key
instance (SingI x, n ~ GetEvDevKey x, KnownNat n) ⇒ AllKeys (x ∷ RowKey) where
  getAllKeys Proxy = singleton k v
    where k = Key $ fromInteger $ natVal (Proxy ∷ Proxy n)
          v = fromSing (sing ∷ Sing x)


allKeys ∷ HashMap Key RowKey
allKeys = getAllKeys (Proxy ∷ Proxy AllKeysRows)
