module GUI.Main where

import qualified Data.List as L

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error
import Control.Exception
import Data.Typeable
import Data.IORef
import Data.Maybe

import Graphics.UI.Gtk hiding(get)
import qualified Graphics.Rendering.Cairo as C
import Control.Concurrent

import Core
import GUI.Render (renderCanvas)

-- | Invalidate a widget so that the expose event will be fired. This causes
-- the widget to get redrawn.
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
main midi = do
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
    ch <- midi
    forkIO $ forever (updateState ch stvar >> (invalidate canvas))
    widgetShowAll window
    mainGUI

