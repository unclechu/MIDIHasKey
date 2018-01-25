{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}

module GUI where

import Prelude hiding (lookup)
import Prelude.Unicode
import GHC.TypeLits

import Data.Proxy
import Data.HashMap.Strict

import Control.Monad

import Graphics.UI.WX
import Sound.MIDI.Message.Channel

-- local
import Types
import Utils
import Keys.Types
import Keys.Specific.GUI


data GUIContext
  = GUIContext
  { noteButtonHandler  ∷ RowKey → Bool → IO ()
  , panicButtonHandler ∷ IO ()
  , getPitchMapping    ∷ IO (HashMap RowKey Pitch)
  }


mainWnd ∷ GUIContext → IO ()
mainWnd ctx = do
  mainFrame ← frameFixed [text := symbolVal (Proxy ∷ Proxy WindowTitle)]
  let sBtn = smallButton mainFrame
  pitchMap ← getPitchMapping ctx

  allButtons ←
    let getButton ∷ GUIKeyOfRow → IO (RowKey, Button ())
        getButton (rowKey, label) = sBtn props <&> (rowKey,)

          where props = [ text       := btnLabel
                        , on click   := const $ noteButtonHandler ctx rowKey True
                        , on unclick := const $ noteButtonHandler ctx rowKey False
                        ]

                btnLabel = case lookup rowKey pitchMap of
                                -- +1 to shift from [0..127] to [1..128]
                                Just x  → label ⧺ fmap superscript (show $ fromPitch x + 1)
                                Nothing → label

     in forM allGUIRows $ mapM getButton

  panicBtn ← sBtn [text := "Panic", on command := panicButtonHandler ctx]

  let buttonsMap ∷ HashMap RowKey (Button ())
      buttonsMap = unions $ fmap fromList allButtons

  set mainFrame
    [ layout := margin 5 $ column 5
        [ widget panicBtn
        , boxed "Keyboard" $ margin 5 $ column 5 $ reverse $
          fmap (hfloatCenter ∘ row 5 ∘ fmap (snd • widget)) allButtons
        ]
    ]


runGUI ∷ GUIContext → IO ()
runGUI = start ∘ mainWnd
