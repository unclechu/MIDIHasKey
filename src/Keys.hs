{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

-- Type-level
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- For 'singletons'
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

module Keys where

import Prelude.Unicode
import GHC.TypeLits

import Data.Proxy
import Data.Word
import Data.Kind
import Data.Singletons.TH
import Data.Type.Equality

-- local
import Utils


singletons [d|
  data RowKey
    = ZKey | XKey | CKey | VKey | BKey | NKey | MKey | CommaKey | DotKey | SlashKey
    | AKey | SKey | DKey | FKey | GKey | HKey | JKey | KKey | LKey | ColonKey | QuoteKey

    | QKey | WKey | EKey | RKey | TKey | YKey | UKey | IKey | OKey | PKey
    | BracketLKey | BracketRKey | BSlashKey

    | BacktickKey | N1Key | N2Key | N3Key | N4Key | N5Key | N6Key | N7Key | N8Key | N9Key | N0Key
    | MinusKey | EqualKey

    deriving (Eq, Show)
    |]


type KeysRow1
  = 'ZKey ↔ 'XKey ↔ 'CKey ↔ 'VKey ↔ 'BKey
  ↔ 'NKey ↔ 'MKey ↔ 'CommaKey ↔ 'DotKey ↔ 'SlashKey
instance RowProxies KeysRow1 where
  rowProxies _ = ( (Proxy ∷ Proxy (RowList KeysRow1))
                 , (Proxy ∷ Proxy (RowOffset KeysRow1))
                 )

type KeysRow2
  = 'AKey ↔ 'SKey ↔ 'DKey ↔ 'FKey ↔ 'GKey
  ↔ 'HKey ↔ 'JKey ↔ 'KKey ↔ 'LKey ↔ 'ColonKey ↔ 'QuoteKey
instance RowProxies KeysRow2 where
  rowProxies _ = ( (Proxy ∷ Proxy (RowList KeysRow2))
                 , (Proxy ∷ Proxy (RowOffset KeysRow2))
                 )

type KeysRow3
  = 'QKey ↔ 'WKey ↔ 'EKey ↔ 'RKey ↔ 'TKey
  ↔ 'YKey ↔ 'UKey ↔ 'IKey ↔ 'OKey ↔ 'PKey
  ↔ 'BracketLKey ↔ 'BracketRKey ↔ 'BSlashKey
instance RowProxies KeysRow3 where
  rowProxies _ = ( (Proxy ∷ Proxy (RowList KeysRow3))
                 , (Proxy ∷ Proxy (RowOffset KeysRow3))
                 )

type KeysRow4
  = 'BacktickKey
  ↔ 'N1Key ↔ 'N2Key ↔ 'N3Key ↔ 'N4Key ↔ 'N5Key
  ↔ 'N6Key ↔ 'N7Key ↔ 'N8Key ↔ 'N9Key ↔ 'N0Key
  ↔ 'MinusKey ↔ 'EqualKey
instance RowProxies KeysRow4 where
  rowProxies _ = ( (Proxy ∷ Proxy (RowList KeysRow4))
                 , (Proxy ∷ Proxy (RowOffset KeysRow4))
                 )


type RowKeyMap
  = '[ '( 'ZKey, "Z" ), '( 'XKey, "X" ), '( 'CKey, "C" ), '( 'VKey, "V" ), '( 'BKey, "B" )
     , '( 'NKey, "N" ), '( 'MKey, "M" ), '( 'CommaKey, "," ), '( 'DotKey, "." ), '( 'SlashKey, "/" )

     , '( 'AKey, "A" ), '( 'SKey, "S" ), '( 'DKey, "D" ), '( 'FKey, "F" ), '( 'GKey, "G" )
     , '( 'HKey, "H" ), '( 'JKey, "J" ), '( 'KKey, "K" ), '( 'LKey, "L" ), '( 'ColonKey, ";" )
     , '( 'QuoteKey, "'" )

     , '( 'QKey, "Q" ), '( 'WKey, "W" ), '( 'EKey, "E" ), '( 'RKey, "R" ), '( 'TKey, "T" )
     , '( 'YKey, "Y" ), '( 'UKey, "U" ), '( 'IKey, "I" ), '( 'OKey, "O" ), '( 'PKey, "P" )
     , '( 'BracketLKey, "[" ), '( 'BracketRKey, "]" ), '( 'BSlashKey, "\\" )

     , '( 'BacktickKey, "`" )
     , '( 'N1Key, "1" ), '( 'N2Key, "2" ), '( 'N3Key, "3" ), '( 'N4Key, "4" ), '( 'N5Key, "5" )
     , '( 'N6Key, "6" ), '( 'N7Key, "7" ), '( 'N8Key, "8" ), '( 'N9Key, "9" ), '( 'N0Key, "0" )
     , '( 'MinusKey, "-" ), '( 'EqualKey, "=" )
     ]

type family LabelByKey (l ∷ [(RowKey, Symbol)]) (k ∷ RowKey) ∷ Symbol where
  LabelByKey ('(k, label) ': _) k = label
  LabelByKey (_ ': t) k = LabelByKey t k

type family ShowK (a ∷ RowKey) ∷ Symbol where
  ShowK key = LabelByKey RowKeyMap key

type family RowList (a ∷ k) ∷ [(RowKey, Symbol)] where
  RowList (x ↔ xs) = '(x, ShowK x) ': RowList xs
  RowList x = '(x, ShowK x) ': '[]

type family KnownList (a ∷ [(RowKey, Symbol)]) ∷ Constraint where
  KnownList '[] = ()
  KnownList ( '(r, s) ': t ) = (SingI r, KnownSymbol s, KnownList t)

type family RowOffset (a ∷ k) ∷ Nat where
  RowOffset KeysRow1 = 0
  RowOffset KeysRow2 = Len KeysRow1
  RowOffset KeysRow3 = Len KeysRow1 + Len KeysRow2
  RowOffset KeysRow4 = Len KeysRow1 + Len KeysRow2 + Len KeysRow3


class KeyRow (a ∷ [(RowKey, Symbol)]) where
  mappedRow ∷ [Word8] → Proxy a → [(RowKey, String, Word8)]
instance KeyRow '[] where
  mappedRow _ Proxy = []
instance (SingI k, KnownSymbol s, KeyRow t) ⇒ KeyRow ('(k, s) ': t) where
  mappedRow (n : ns) Proxy
    = (fromSing (sing ∷ Sing k), symbolVal (Proxy ∷ Proxy s), n)
    : mappedRow ns (Proxy ∷ Proxy t)

class RowProxies a where
  rowProxies ∷ (KnownList l, KnownNat n, l ~ RowList a, n ~ RowOffset a) ⇒ a → (Proxy l, Proxy n)


getRow ∷ (KeyRow l, KnownNat o) ⇒ Word8 → (Proxy l, Proxy o) → [(RowKey, String, Word8)]
getRow n (listProxy, offsetProxy) = mappedRow [(n + nat2MidiKey offsetProxy)..] listProxy

type OneRow = [(RowKey, String, Word8)]