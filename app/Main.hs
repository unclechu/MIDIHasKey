{-# LANGUAGE UnicodeSyntax #-}

import Prelude.Unicode
import GHC.TypeLits
import Data.Proxy

import Data.Word
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
  mainFrame ← frameFixed [text := "MIDIHasKey - Virtual MIDI keyboard for microtonal music"]

  let getBtns (rowKey, label, midi)
        = smallButton mainFrame [text := label ⧺ map superscript (show midi)]

  btns1 ← forM row1 getBtns
  btns2 ← forM row2 getBtns
  btns3 ← forM row3 getBtns
  btns4 ← forM row4 getBtns

  set mainFrame [layout := margin 5 $ boxed "Keyboard"
                                    $ margin 5
                                    $ column 5 [ hfloatCenter $ row 5 $ map widget btns4
                                               , hfloatCenter $ row 5 $ map widget btns3
                                               , hfloatCenter $ row 5 $ map widget btns2
                                               , hfloatCenter $ row 5 $ map widget btns1
                                               ]]

  putStrLn "going well…"

-- main = mainStereo $ pure ∘ (process *** process)
-- process ∷ Sample → Sample
-- process = id
