module GUI.Render where

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

foreach :: (Monad m) => [a] -> (a -> m b) -> m [b]
foreach = flip mapM

-- | First try to write a function that renders a piano on the screen. Still
-- very ugly duckling, needs rewrite.
renderKeyboard keys (w, h) = do
    let keysize = realToFrac w / 7
        blackheight = realToFrac h * 2 / 3
        br1 = [Db,Eb]
        br2 = [Gb,Ab,Bb]
        wr = [C,D,E,F,G,A,B]
        whitecolor key = if (key `elem` keys) then 1.0 else 0.5
        blackcolor key = if (key `elem` keys) then 1.0 else 0.0
        drawKey x y w_ h_ c = do
                                C.moveTo x y
                                C.lineTo (x + w_) y
                                C.lineTo (x + w_) (y + h_)
                                C.lineTo x (y + h_)
                                C.closePath
                                C.setSourceRGBA 1.0 1.0 c 1.0
                                C.fillPreserve
                                C.setSourceRGBA 0.0 0.0 0.0 1.0
                                C.stroke
 
    C.setLineCap C.LineCapRound
    C.setLineJoin C.LineJoinRound
    C.setLineWidth 2

    foreach ([0 .. 6] :: [Int]) $ \ x ->
        do let col = whitecolor $ wr !! x
           drawKey (realToFrac x * keysize) 0 keysize h col

    foreach ([0 .. 1] :: [Int]) $ \ x ->
        do let col = blackcolor $ br1 !! x
               shiftx = keysize / 2
           drawKey (realToFrac x * keysize + shiftx) 0 keysize blackheight col

    foreach ([0 .. 2] :: [Int]) $ \ x ->
        do let col = blackcolor $ br2 !! x
               shiftx = keysize * 3 + keysize / 2
           drawKey (realToFrac x * keysize + shiftx) 0 keysize blackheight col


renderCanvas st (w, h) = do
    C.setSourceRGBA 0 0 0 1.0
    C.moveTo 10 250; C.showText $ "Currently pressed keys: "
    C.moveTo 150 250; C.showText $ show st
    C.moveTo 10 270; C.showText $ "Matching chords: "
    C.moveTo 150 270; C.showText $ show (matchingChords st)
    renderKeyboard st (300, 200)


