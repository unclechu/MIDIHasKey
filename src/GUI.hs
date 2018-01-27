{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GUI
     ( runGUI
     , GUIContext (..)
     , GUIInterface (..)
     , KeyButtonStateUpdater
     ) where

import Prelude hiding (lookup)
import Prelude.Unicode
import GHC.TypeLits

import Data.Proxy
import Data.Maybe
import Data.HashMap.Strict

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

import Graphics.UI.WX
import Sound.MIDI.Message.Channel

-- local
import Types
import Utils
import Keys.Types
import Keys.Specific.GUI


data GUIContext
  = GUIContext
  { getPitchMapping    ∷ IO (HashMap RowKey Pitch)
  , appExitHandler     ∷ IO ()
  , panicButtonHandler ∷ IO ()
  , noteButtonHandler  ∷ RowKey → Bool → IO ()
  }

data GUIInterface
  = GUIInterface
  { keyButtonStateUpdate ∷ KeyButtonStateUpdater
  }

type KeyButtonStateUpdater = RowKey → Bool → IO ()


mainWnd ∷ GUIContext → MVar (RowKey, Bool) → IO ()
mainWnd ctx keyBtnStateBus = do
  mainFrame ← frameFixed [text := symbolVal (Proxy ∷ Proxy WindowTitle)]
  let sBtn = smallButton mainFrame
  pitchMap ← getPitchMapping ctx

  allButtons ←
    let getButton ∷ GUIKeyOfRow → IO (RowKey, Button ())
        getButton (rowKey, label) = sBtn props <&> (rowKey,)

          where props = [ text       := btnLabel
                        , on click   := const $ noteButtonHandler ctx rowKey True  >> propagateEvent
                        , on unclick := const $ noteButtonHandler ctx rowKey False >> propagateEvent
                        , bgcolor    := keyBtnBgColor
                        , color      := keyBtnFgColor
                        ]

                btnLabel = case lookup rowKey pitchMap of
                                -- +1 to shift from [0..127] to [1..128]
                                Just x  → label ⧺ fmap superscript (show $ fromPitch x + 1)
                                Nothing → label

     in forM allGUIRows $ mapM getButton

  panicBtn ← sBtn [text := "Panic", on command := panicButtonHandler ctx]
  exitBtn  ← sBtn [text := "Exit",  on command := appExitHandler ctx]

  let buttonsMap ∷ HashMap RowKey (Button ())
      buttonsMap = unions $ fmap fromList allButtons

  set mainFrame
    [ layout := margin 5 $ column 5
        [ row 5 [widget panicBtn, widget exitBtn]
        , boxed "Keyboard" $ margin 5 $ column 5 $ reverse $
          fmap (hfloatCenter ∘ row 5 ∘ fmap (snd • widget)) allButtons
        ]
    ]

  void $ forkIO $ catchThreadFail "GUI listener for key button state updates" $ forever $ do
    (rowKey, isPressed) ← takeMVar keyBtnStateBus

    pure ()
    {- TODO FIXME it fails with segfaults :( maybe because `set` here isn't thread-safe
    fromMaybe (pure ()) $
      rowKey `lookup` buttonsMap
        <&> flip set [ bgcolor := if isPressed then keyBtnPressedBgColor else keyBtnBgColor
                     , color   := if isPressed then keyBtnPressedFgColor else keyBtnFgColor
                     ]
    -}


runGUI ∷ GUIContext → IO GUIInterface
runGUI ctx = do
  (keyBtnStateBus ∷ MVar (RowKey, Bool)) ← newEmptyMVar
  void $ forkIO $ catchThreadFail "Main GUI" $ start $ mainWnd ctx keyBtnStateBus
  pure GUIInterface { keyButtonStateUpdate = curry $ putMVar keyBtnStateBus }

keyBtnBgColor, keyBtnFgColor, keyBtnPressedBgColor, keyBtnPressedFgColor ∷ Color
keyBtnBgColor        = colorSystem ColorBackground
keyBtnFgColor        = colorSystem ColorBtnText
keyBtnPressedBgColor = colorSystem ColorHighlight
keyBtnPressedFgColor = colorSystem ColorHighlightText
