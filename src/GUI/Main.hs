module GUI.Main where

import qualified Data.List as L
import Control.Monad.State
import Control.Concurrent
import Graphics.UI.Gtk

import qualified Sound.MIDI.Message as Msg
import Sound.MIDI.Message.Channel (T (Cons), messageBody, Body (Voice))
import qualified Sound.MIDI.Message.Channel.Voice as V

import Core
import GUI.Render (renderCanvas)

historyLength = 40

-- | The core of this module's functionality: Take a MIDI message and a list
-- of keys, and return the resulting list. Pressing a key will add the
-- corresponding note to the list, lifting it will delete it.
transform :: Msg.T -> [Note] -> [Note]
transform (Msg.Channel Cons {messageBody = (Voice (V.NoteOn p _))})
            = (:) $ fromPitch p
transform (Msg.Channel Cons {messageBody = (Voice (V.NoteOff p _))})
            = L.delete $ fromPitch p
transform _ = id

-- | If a note is pressed, add it to the history.
record :: Msg.T -> [Note] -> [Note]
record (Msg.Channel Cons {messageBody = (Voice (V.NoteOn p _))})
            = ((:) $ fromPitch p) . take historyLength
record _ = id

-- | For every message that is read from the Chan, the state that is stored
-- in the MVar stvar is updated.
updateState ch stvar = do
    msg <- readChan ch
    modifyMVar_ stvar $ return . (\(c, h) -> (transform msg c, record msg h))
    (_,history) <- readMVar stvar
    putStrLn (show (histogram history))

-- | Invalidate a widget so that the expose event will be fired. This causes
-- the widget to get redrawn.
invalidate :: DrawingArea -> IO ()
invalidate win = do
    dwin <- widgetGetDrawWindow win
    drawWindowInvalidateRect dwin rect False
    return ()
    where rect = (Rectangle 0 0 1000 1000) -- FIXME

-- | Main function. Create a GTK window with a drawing window, fire up the
-- MIDI bridge and register the expose rendering handler.
main midi = do
    stvar <- newMVar ([], [])
    initGUI
    window <- windowNew
    canvas <- drawingAreaNew
    set window [ containerChild := canvas ]
    onDestroy window mainQuit
    onExpose canvas (\e -> do state <- readMVar stvar
                              size <- widgetGetSize canvas
                              drawing <- widgetGetDrawWindow canvas
                              renderWithDrawable drawing $ renderCanvas state
                              return True)
    ch <- midi
    forkIO $ forever (updateState ch stvar >> (invalidate canvas))
    widgetShowAll window
    mainGUI

