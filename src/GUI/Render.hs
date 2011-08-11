module GUI.Render where

import qualified Graphics.Rendering.Cairo as C
import qualified Data.Maybe as M
import qualified Data.List as L

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV

import Core
import GUI.Theme

foreach :: (Monad m) => [a] -> (a -> m b) -> m [b]
foreach = flip mapM

centerShowText :: String -> Double -> Double -> C.Render ()
centerShowText s cx cy = do
            te <- C.textExtents s
            C.moveTo (cx - (C.textExtentsWidth te)/2) (cy + (C.textExtentsHeight te)/2)
            C.showText s
            return ()

renderArc :: Double     -- Center x coordinate
            -> Double   -- Center y coordinate
            -> Note     -- Note in the Circle of Fifths
            -> (Note -> Bool -> RGB Double)
            -> C.Render ()
renderArc cx cy key colorfunc = do
    let radians = (noteToCOFAngle key) * pi / 180.0 :: Double
        radiansL = (radians - (pi / 2) - 15 * pi / 180.0)
        radiansR = (radians - (pi / 2) + 15 * pi / 180.0)
        RGB majr majg majb = colorfunc key True
        RGB minr ming minb = colorfunc (key `add` MajorSixth) False
        arc min max = do
            C.newPath
            C.arc cx cy max radiansL radiansR
            C.arcNegative cx cy min radiansR radiansL
            C.closePath
        point dist gamma = (x, y)
            where x = cx + (sin (radians + gamma)) * dist
                  y = cy - (cos (radians + gamma)) * dist

    C.setLineWidth 0.8

    -- Render major part of the arc
    arc (cx - 40) cx
    C.setSourceRGBA majr majg majb 1.0
    C.fillPreserve
    C.setSourceRGBA 0.0 0.0 0.0 1.0
    C.stroke

    -- Render minor part of the arc
    arc (cx - 80) (cx - 40)
    C.setSourceRGBA minr ming minb 1.0
    C.fillPreserve
    C.setSourceRGBA 0.0 0.0 0.0 1.0
    C.stroke

    -- Paint thick border around the arc
    C.setLineWidth 2
    C.newPath
    C.arc cx cy cx radiansL radiansR
    C.stroke
    C.newPath
    C.arc cx cy (cx - 80) radiansL radiansR
    C.stroke

    -- Render chord names
    C.setFontSize 16
    uncurry (centerShowText (show key)) $ point (cx - 20) 0
    C.setFontSize 12
    uncurry (centerShowText $ show (key `add` MajorSixth) ++ "m") $ point (cx - 60) 0


-- | Render Circle of Fifths
renderCOF colorfunc (w, h) = do
    let cx = realToFrac w / 2
        cy = realToFrac h / 2
    C.setLineCap C.LineCapRound
    C.setLineJoin C.LineJoinRound
    foreach (take 12 $ circleOfFifths C) $ \ note -> renderArc cx cy note colorfunc

-- | First try to write a function that renders a piano on the screen. Still
-- very ugly duckling, needs rewrite.
renderKeyboard colorfunc (w, h) = do
    let keysize = realToFrac w / 7
        blackheight = realToFrac h * 2 / 3
        br1 = [C',D']
        br2 = [F',G',A']
        wr = [C,D,E,F,G,A,B]
        drawKey x y w_ h_ (RGB r g b) = do
                                C.rectangle x y w_ h_
                                C.setSourceRGBA r g b 1.0
                                C.fillPreserve
                                C.setSourceRGBA 0.0 0.0 0.0 1.0
                                C.stroke
 
    C.setLineCap C.LineCapRound
    C.setLineJoin C.LineJoinRound
    C.setLineWidth 1.2

    foreach ([0 .. 6] :: [Int]) $ \ x ->
        drawKey (realToFrac x * keysize) 0 keysize h (colorfunc $ wr !! x)

    foreach ([0 .. 1] :: [Int]) $ \ x ->
        do let shiftx = keysize / 2 + (keysize - keysize / 1.3) / 2
           drawKey (realToFrac x * keysize + shiftx) 0 (keysize / 1.3) blackheight (colorfunc $ br1 !! x)

    foreach ([0 .. 2] :: [Int]) $ \ x ->
        do let shiftx = keysize * 3.5 + (keysize - keysize / 1.3) / 2
           drawKey (realToFrac x * keysize + shiftx) 0 (keysize / 1.3) blackheight (colorfunc $ br2 !! x)



renderCanvas (currentNotes, historyNotes) = do

    C.setSourceRGBA 0 0 0 1.0

    C.setFontSize 14
    C.moveTo 10 20;     C.showText "Current notes:"
    C.moveTo 270 20;    C.showText "Current chord:"
    C.moveTo 550 20;    C.showText "Current scale:"
    C.moveTo 10 330;    C.showText "All chords with these notes:"

    C.setFontSize 20
    C.moveTo 10 50;     C.showText $ niceList 12 (map show currentNotes)
    C.moveTo 270 50;    C.showText $ niceList 1 (map show (chordsWithExactNotes currentNotes))
    C.moveTo 550 50;    C.showText $ niceList 1 (map show (scalesWithNotes historyNotes))
    C.moveTo 10 360;    C.showText $ niceList 10 (map show (chordsWithNotes currentNotes))

    C.translate 10 70;  renderKeyboard (grayKeyboard currentNotes) (250, 180)
    C.translate 270 0;  renderCOF (grayCOF currentNotes) (250, 250)
    C.translate 270 0;  renderKeyboard (grayDistributionKeyboard historyNotes) (250, 180)

    where niceList n lst = unwords (take n lst)

