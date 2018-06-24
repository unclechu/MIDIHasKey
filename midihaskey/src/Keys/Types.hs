{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveGeneric #-}

-- Type-level
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

-- For 'singletons'
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

module Keys.Types where

import Prelude.Unicode
import GHC.TypeLits
import GHC.Generics (Generic)

import Data.Singletons.TH
import Data.Hashable (Hashable)

-- local
import MIDIHasKey.Utils (type (↔))


singletons [d|
  data RowKey
    = ZKey | XKey | CKey | VKey | BKey | NKey | MKey | CommaKey | DotKey | SlashKey
    | AKey | SKey | DKey | FKey | GKey | HKey | JKey | KKey | LKey | ColonKey | QuoteKey

    | QKey | WKey | EKey | RKey | TKey | YKey | UKey | IKey | OKey | PKey
    | BracketLKey | BracketRKey | BSlashKey

    | BacktickKey | N1Key | N2Key | N3Key | N4Key | N5Key | N6Key | N7Key | N8Key | N9Key | N0Key
    | MinusKey | EqualKey

    deriving (Show, Read, Eq, Generic)
    |]

instance Hashable RowKey


type AllKeysRows = '[KeysRow1, KeysRow2, KeysRow3, KeysRow4]

type KeysRow1
  = 'ZKey ↔ 'XKey ↔ 'CKey ↔ 'VKey ↔ 'BKey
  ↔ 'NKey ↔ 'MKey ↔ 'CommaKey ↔ 'DotKey ↔ 'SlashKey

type KeysRow2
  = 'AKey ↔ 'SKey ↔ 'DKey ↔ 'FKey ↔ 'GKey
  ↔ 'HKey ↔ 'JKey ↔ 'KKey ↔ 'LKey ↔ 'ColonKey ↔ 'QuoteKey

type KeysRow3
  = 'QKey ↔ 'WKey ↔ 'EKey ↔ 'RKey ↔ 'TKey
  ↔ 'YKey ↔ 'UKey ↔ 'IKey ↔ 'OKey ↔ 'PKey
  ↔ 'BracketLKey ↔ 'BracketRKey ↔ 'BSlashKey

type KeysRow4
  = 'BacktickKey
  ↔ 'N1Key ↔ 'N2Key ↔ 'N3Key ↔ 'N4Key ↔ 'N5Key
  ↔ 'N6Key ↔ 'N7Key ↔ 'N8Key ↔ 'N9Key ↔ 'N0Key
  ↔ 'MinusKey ↔ 'EqualKey


-- (RowKey, Label, Linux device key number)
type RowKeyMap
  = '[ '( 'ZKey, "Z", 44 ), '( 'XKey, "X", 45 ), '( 'CKey, "C", 46 ), '( 'VKey, "V", 47 )
     , '( 'BKey, "B", 48 ), '( 'NKey, "N", 49 ), '( 'MKey, "M", 50 )
     , '( 'CommaKey, ",", 51 ), '( 'DotKey, ".", 52 ), '( 'SlashKey, "/", 53 )

     , '( 'AKey, "A", 30 ), '( 'SKey, "S", 31 ), '( 'DKey, "D", 32 ), '( 'FKey, "F", 33 )
     , '( 'GKey, "G", 34 ), '( 'HKey, "H", 35 ), '( 'JKey, "J", 36 ), '( 'KKey, "K", 37 )
     , '( 'LKey, "L", 38 ), '( 'ColonKey, ";", 39 ), '( 'QuoteKey, "'", 40 )

     , '( 'QKey, "Q", 16 ), '( 'WKey, "W", 17 ), '( 'EKey, "E", 18 ), '( 'RKey, "R", 19 )
     , '( 'TKey, "T", 20 ), '( 'YKey, "Y", 21 ), '( 'UKey, "U", 22 ), '( 'IKey, "I", 23 )
     , '( 'OKey, "O", 24 ), '( 'PKey, "P", 25 )
     , '( 'BracketLKey, "[", 26 ), '( 'BracketRKey, "]", 27 ), '( 'BSlashKey, "\\", 43 )

     , '( 'BacktickKey, "`", 41 )
     , '( 'N1Key, "1", 2 ), '( 'N2Key, "2", 3 ), '( 'N3Key, "3", 4 ), '( 'N4Key, "4", 5 )
     , '( 'N5Key, "5", 6 ), '( 'N6Key, "6", 7 ), '( 'N7Key, "7", 8 ), '( 'N8Key, "8", 9 )
     , '( 'N9Key, "9", 10 ), '( 'N0Key, "0", 11 ), '( 'MinusKey, "-", 12 ), '( 'EqualKey, "=", 13 )
     ]
