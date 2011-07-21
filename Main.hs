module Main where

import qualified Data.List as L

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error
import Control.Exception
import Data.Typeable
import Data.IORef

import Graphics.UI.Gtk hiding(get)
import Graphics.Rendering.Cairo hiding(scale,transform)
import Control.Concurrent

import Core
import qualified MIDIBridge

import Data.Maybe

renderCanvas st (w, h) = do
    setSourceRGBA 0 0 0 0.5
    setLineWidth 2.5
    stroke
    moveTo 10 10
    lineTo 20 20
    showText $ "Current keys: " ++ (show st) ++ "; Canvas size: " ++ (show (w, h))

invalidate :: DrawingArea -> IO ()
invalidate win = do
    dwin <- widgetGetDrawWindow win
    drawWindowInvalidateRect dwin rect False
    return ()
    where rect = (Rectangle 0 0 1000 1000) -- FIXME

-- | For every message that is read from the Chan, the state that is stored
-- in the MVar stvar is updated.
updateState ch stvar = do
    msg <- readChan ch
    modifyMVar_ stvar $ return . (transform msg)

-- | Main function. Create a GTK window with a drawing window, fire up the
-- MIDI bridge and register the expose rendering handler.
main = do
    stvar <- newMVar []
    initGUI
    window <- windowNew
    canvas <- drawingAreaNew
    set window [ containerChild := canvas ]
    onDestroy window mainQuit
    onExpose canvas (\e -> do st <- readMVar stvar
                              size <- widgetGetSize canvas
                              drawing <- widgetGetDrawWindow canvas
                              renderWithDrawable drawing $ renderCanvas st size
                              return True)
    ch <- MIDIBridge.run
    forkIO $ forever (updateState ch stvar >> (invalidate canvas))
    widgetShowAll window
    mainGUI

