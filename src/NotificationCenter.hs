{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module NotificationCenter where

import Data.Maybe
import qualified Data.Text as Text
import Control.Monad.Trans.Reader (ReaderT(..))
import GI.Gtk as Gtk
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Types (Cairo(..))
import Graphics.Rendering.Cairo.Internal (Render(..))
import GI.Cairo
import Foreign.Ptr (castPtr)

main :: IO ()
main = do
  Gtk.init Nothing
  let objsToGet = [ "main_window" , "main_bg" ]
  builder <- Gtk.builderNew
  Gtk.builderAddFromFile builder "crash.glade"

  (Just win) <- Gtk.builderGetObject builder "main_window"
  (Just bg) <- Gtk.builderGetObject builder "main_bg"

  mainWindow <- (Gtk.unsafeCastTo Gtk.Window) $ win
  drawingArea <- (Gtk.unsafeCastTo Gtk.DrawingArea) $ bg

  onWidgetDraw drawingArea $ \(Context fp) -> withManagedPtr fp $ \p -> (`runReaderT` Cairo (castPtr p)) $ runRender $ do
    return True

  onWidgetDestroy mainWindow mainQuit
  widgetShowAll mainWindow
  Gtk.main
