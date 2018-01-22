{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}

module GUI where

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
  { allRows            ∷ [GUIKeysRow]
  , noteButtonHandler  ∷ RowKey → Pitch → Bool → IO ()
  , panicButtonHandler ∷ IO ()
  }


mainWnd ∷ GUIContext → IO ()
mainWnd ctx = do
  mainFrame ← frameFixed [text := symbolVal (Proxy ∷ Proxy WindowTitle)]
  let sBtn = smallButton mainFrame

  allButtons ←
    let getButton ∷ GUIKeyOfRow → IO (RowKey, Button ())
        getButton (rowKey, label, midi)
          = sBtn props <&> (rowKey,)
          where props = [ text       := label ⧺ fmap superscript (show $ fromPitch midi)
                        , on click   := const $ noteButtonHandler ctx rowKey midi True
                        , on unclick := const $ noteButtonHandler ctx rowKey midi False
                        ]

     in mapM (mapM getButton) $ allRows ctx

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
