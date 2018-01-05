{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}

module GUI where

import Prelude.Unicode
import GHC.TypeLits

import Data.Word
import Data.HashMap.Strict
import Control.Monad

import Graphics.UI.WX

-- local
import Utils
import Keys


data GUIContext
  = GUIContext
  { allRows ∷ [OneRow]
  , buttonHandler ∷ RowKey → Word8 → IO ()
  }


mainWnd ∷ GUIContext → IO ()
mainWnd ctx = do
  mainFrame ← frameFixed [text := "MIDIHasKey — Virtual MIDI keyboard for microtonal music"]

  allButtons ←
    let getButton ∷ RowEl → IO (RowKey, Button ())
        getButton (rowKey, label, midi)
          = smallButton mainFrame props <&> (rowKey,)
          where props = [ text       := label ⧺ fmap superscript (show midi)
                        , on command := buttonHandler ctx rowKey midi
                        ]

     in mapM (mapM getButton) $ allRows ctx

  let buttonsMap ∷ HashMap RowKey (Button ())
      buttonsMap = unions $ fmap fromList allButtons

  set mainFrame
    [ layout := margin 5 $ boxed "Keyboard"
                         $ margin 5
                         $ column 5
                         $ reverse
                         $ fmap (hfloatCenter ∘ row 5 ∘ fmap (snd • widget)) allButtons
    ]


runGUI ∷ GUIContext → IO ()
runGUI = start ∘ mainWnd
