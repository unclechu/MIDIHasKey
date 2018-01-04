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

      row1 = getRow startMidiKey $ rowProxies ((⊥) ∷ KeysRow1)
      row2 = getRow startMidiKey $ rowProxies ((⊥) ∷ KeysRow2)
      row3 = getRow startMidiKey $ rowProxies ((⊥) ∷ KeysRow3)
      row4 = getRow startMidiKey $ rowProxies ((⊥) ∷ KeysRow4)

  start $ mainWnd (row1, row2, row3, row4)
  return ()


mainWnd ∷ (OneRow, OneRow, OneRow, OneRow) → IO ()
mainWnd (row1, row2, row3, row4) = do
  mainFrame ← frameFixed [text := "MIDIHasKey — Virtual MIDI keyboard for microtonal music"]

  let getBtn ∷ RowEl → IO (RowKey, Button ())
      getBtn (rowKey, label, midi)
        = smallButton mainFrame [text := label ⧺ fmap superscript (show midi)] <&> (rowKey,)

      getRowBtns ∷ OneRow → IO (HashMap RowKey (Button ()))
      getRowBtns = mapM getBtn • fmap fromList

  btns1 ← getRowBtns row1
  btns2 ← getRowBtns row2
  btns3 ← getRowBtns row3
  btns4 ← getRowBtns row4

  let allBtns ∷ HashMap RowKey (Button ())
      allBtns = unions [btns1, btns2, btns3, btns4]

  print allBtns

  set mainFrame [layout := margin 5 $ boxed "Keyboard"
                                    $ margin 5
                                    $ column 5 [ hfloatCenter $ row 5 $ elems $ fmap widget btns4
                                               , hfloatCenter $ row 5 $ elems $ fmap widget btns3
                                               , hfloatCenter $ row 5 $ elems $ fmap widget btns2
                                               , hfloatCenter $ row 5 $ elems $ fmap widget btns1
                                               ]]

  putStrLn "going well…"

-- main = mainStereo $ pure ∘ (process *** process)
-- process ∷ Sample → Sample
-- process = id
