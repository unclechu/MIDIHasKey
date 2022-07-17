{-# LANGUAGE UnicodeSyntax #-}

-- Type-level
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Keys.Specific.EventHandler
     ( allKeysOrder
     ) where

import Data.Proxy
import Data.Singletons.TH

-- local
import Keys.Types
import MIDIHasKey.Utils (type (↔), (⋄))


class AllKeysOrder a where
  getAllKeysOrder ∷ Proxy a → [RowKey]

instance AllKeysOrder '[] where
  getAllKeysOrder Proxy = []

instance (AllKeysOrder h, AllKeysOrder t) ⇒ AllKeysOrder (h ': t) where
  getAllKeysOrder Proxy = getAllKeysOrder (Proxy ∷ Proxy h) ⋄ getAllKeysOrder (Proxy ∷ Proxy t)

instance (AllKeysOrder h, AllKeysOrder t) ⇒ AllKeysOrder (h ↔ t) where
  getAllKeysOrder Proxy = getAllKeysOrder (Proxy ∷ Proxy h) ⋄ getAllKeysOrder (Proxy ∷ Proxy t)

instance (SingI x) ⇒ AllKeysOrder (x ∷ RowKey) where
  getAllKeysOrder Proxy = [fromSing (sing ∷ Sing x)]


allKeysOrder ∷ [RowKey]
allKeysOrder = getAllKeysOrder (Proxy ∷ Proxy AllKeysRows)
