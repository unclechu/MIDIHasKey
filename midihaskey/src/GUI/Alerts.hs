{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}

module GUI.Alerts where

import Prelude
import Prelude.Unicode

import Data.Text (type Text)

import Control.Monad
import Control.Concurrent.MVar

import Graphics.UI.Gtk

-- local
import Types


guiAlerts ∷ MVar AlertMessage → Window → IO ()
guiAlerts alertsBus wnd = forever $
  takeMVar alertsBus >>=
    \case InfoAlert  msg → showDialog MessageInfo  msg
          ErrorAlert msg → showDialog MessageError msg
  where
    showDialog ∷ MessageType → Text → IO ()
    showDialog msgType msg = postGUIAsync $ do
      w ← messageDialogNew (Just wnd) dialogFlags msgType ButtonsOk msg
      _ ← dialogRun w
      widgetDestroy w

    dialogFlags = [DialogModal, DialogDestroyWithParent]
