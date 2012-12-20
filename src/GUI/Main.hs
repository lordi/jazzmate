module GUI.Main where

import qualified Data.List as L
import qualified Data.Maybe as M

import Graphics.UI.Gtk

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Control.Monad.State

import qualified Sound.MIDI.Message as Msg
import Sound.MIDI.Message.Channel (T (Cons), messageBody, Body (Voice))
import qualified Sound.MIDI.Message.Channel.Voice as V

import Core
import GUI.Render (renderCanvas)
import MusicTheory

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
            = take historyLength . ((:) $ fromPitch p)
record _ = id

-- | For every message that is read from the Chan, the state that is stored
-- in the MVar stvar is updated.
waitAndUpdateState noteCh stvar = do
    msg <- atomically $ readTChan noteCh
    modifyMVar_ stvar $ return . (\(c, h, s) -> (transform msg c, record msg h, s))
    -- (_,history) <- readMVar stvar
    -- putStrLn (show (guessScales history))

-- | Invalidate a widget so that the expose event will be fired. This causes
-- the widget to get redrawn.
invalidate :: DrawingArea -> IO ()
invalidate win = do
    dwin <- widgetGetDrawWindow win
    drawWindowInvalidateRect dwin rect False
    return ()
    where rect = (Rectangle 0 0 1000 1000) -- FIXME

makeTextComboBox :: [String] -> Maybe String -> IO ComboBox
makeTextComboBox texts defaulttext = do
  cb <- comboBoxNewText
  comboBoxAppendText cb ""
  mapM_ (comboBoxAppendText cb) texts
  let active = maybe 0 (\def -> 1 + (M.fromJust $ L.elemIndex def texts)) defaulttext
  comboBoxSetActive cb active
  return cb

-- | Main function. Create a GTK window with a drawing window, fire up the
-- MIDI bridge and register the expose rendering handler.
main midiProvider renderFunc initialScale = do
    stvar <- newMVar ([], [], initialScale)
    noteCh <- newTChanIO

    initGUI
    window  <- windowNew
    vbox    <- vBoxNew False 10

--  hbox    <- hBoxNew True 10
--  label <- labelNew (Just "MIDI input:")
--  combo <- (makeTextComboBox $ map show [1,2,3])
--  containerAdd hbox label
--  containerAdd hbox combo
--  containerAdd vbox hbox

    hbox2   <- hBoxNew True 10
    label2 <- labelNew (Just "Current scale:")
    let allscales = [Scale n s | n <- [C ..], s <- [MajorPentatonic ..]]
    scaleCombo <- makeTextComboBox (map show allscales) (fmap show initialScale)
    
    boxPackStart hbox2 label2 PackNatural 10
    boxPackStart hbox2 scaleCombo PackNatural 10
--    containerAdd hbox2 label2
--    containerAdd hbox2 scaleCombo
    --containerAdd vbox hbox2
    boxPackStart vbox hbox2 PackNatural 10

    canvas <- drawingAreaNew
    containerAdd vbox canvas
    set window [ containerChild := vbox ]
    onDestroy window mainQuit
    onExpose canvas (\e -> do state <- readMVar stvar
                              size <- widgetGetSize canvas
                              drawing <- widgetGetDrawWindow canvas
                              renderWithDrawable drawing $ renderFunc state
                              return True)

    scaleCombo `on` changed $ do
        index <- comboBoxGetActive scaleCombo
        let maxmode = 1 + fromEnum (maxBound :: ScaleType)
            note = (toEnum ((index - 1) `div` maxmode)) :: Note
            mode = (toEnum ((index - 1) `mod` maxmode)) :: ScaleType
            selectedScale = Scale note mode
        putStrLn $ show selectedScale
        modifyMVar_ stvar $ return . (\(c, h, _) -> (c, h, Just selectedScale))
        invalidate canvas

    forkIO $ forever (waitAndUpdateState noteCh stvar >> (invalidate canvas))
    forkIO $ midiProvider noteCh

    widgetShowAll window
    mainGUI

