{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ExplicitNamespaces #-}

module GUI.Utils where

import Prelude
import Prelude.Unicode

import System.Glib.UTFString
import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.General.CssProvider as GtkCssProvider (type CssProvider)
import Graphics.UI.Gtk.General.CssProvider hiding (type CssProvider)
import Graphics.UI.Gtk.General.StyleContext


type CssProvider = GtkCssProvider.CssProvider

getCssProvider ∷ IO CssProvider
getCssProvider = do
  cssProvider ← cssProviderNew
  cssProvider <$ cssProviderLoadFromPath cssProvider "./gtk-custom.css"

-- Priority range is [1..800]. See also:
-- https://www.stackage.org/haddock/lts-9.21/gtk3-0.14.8/src/Graphics.UI.Gtk.General.StyleContext.html#styleContextAddProvider
maxCssPriority ∷ Int
maxCssPriority = 800

bindCssProvider ∷ WidgetClass widget ⇒ CssProvider → widget → IO StyleContext
bindCssProvider cssProvider w = do
  styleContext ← widgetGetStyleContext w
  styleContext <$ styleContextAddProvider styleContext cssProvider maxCssPriority

withCssClass ∷ (WidgetClass w, GlibString s) ⇒ CssProvider → s → w → IO StyleContext
withCssClass cssProvider className w = do
  styleContext ← bindCssProvider cssProvider w
  styleContext <$ styleContextAddClass styleContext className
