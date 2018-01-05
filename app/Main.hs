{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}

import Prelude.Unicode
import GHC.TypeLits
import Data.Proxy

import Data.Word
import Data.HashMap.Strict
import Control.Arrow
import Control.Concurrent
import Control.Monad

import Sound.JACK.Audio

import Graphics.UI.WX

-- local
import Utils
import Keys


main = do
  let startMidiKey = 20 ∷ Word8
      allRows = getAllRows startMidiKey (Proxy ∷ Proxy AllRows)

  start $ mainWnd allRows


mainWnd ∷ [OneRow] → IO ()
mainWnd allRows = do
  mainFrame ← frameFixed [text := "MIDIHasKey — Virtual MIDI keyboard for microtonal music"]

  let getButton ∷ RowEl → IO (RowKey, Button ())
      getButton (rowKey, label, midi)
        = smallButton mainFrame [text := label ⧺ fmap superscript (show midi)] <&> (rowKey,)

      getRowButtons ∷ OneRow → IO (HashMap RowKey (Button ()))
      getRowButtons = mapM getButton • fmap fromList

  allButtons ← mapM getRowButtons allRows

  let buttonsMap ∷ HashMap RowKey (Button ())
      buttonsMap = unions allButtons

  print buttonsMap

  set mainFrame [ layout := margin 5 $ boxed "Keyboard"
                                     $ margin 5
                                     $ column 5
                                     $ reverse
                                     $ fmap (hfloatCenter ∘ row 5 ∘ elems ∘ fmap widget) allButtons
                ]

  putStrLn "going well…"

-- main = mainStereo $ pure ∘ (process *** process)
-- process ∷ Sample → Sample
-- process = id
