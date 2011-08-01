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
{-
renderArc :: Double     -- Center x coordinate
            -> Double   -- Center y coordinate
            -> Key      -- Key in the Circle of Fifths
            -> (Key -> Bool -> RGB Double)
            -> C.Render ()
renderArc cx cy key colorfunc = do
    let radians = (keyToCOFAngle key) * pi / 180.0 :: Double
        radiansL = (radians - (pi / 2) - 15 * pi / 180.0)
        radiansR = (radians - (pi / 2) + 15 * pi / 180.0)
        RGB majr majg majb = colorfunc key True
        RGB minr ming minb = colorfunc (key `down` minor_third) False
        arc min max = do
            C.newPath
            C.arc cx cy max radiansL radiansR
            C.arcNegative cx cy min radiansR radiansL
            C.closePath
        point dist gamma = (x, y)
            where x = cx + (sin (radians + gamma)) * dist
                  y = cy - (cos (radians + gamma)) * dist

    -- Render major part of the arc
    arc (cx - 50) (cx - 10)
    C.setSourceRGBA majr majg majb 1.0
    C.fillPreserve

    -- Render minor part of the arc
    arc (cx - 95) (cx - 50)
    C.setSourceRGBA minr ming minb 1.0
    C.fillPreserve

    -- Render chord names
    C.setSourceRGBA 0.0 0.0 0.0 1.0
    uncurry (centerShowText (show key)) $ point (cx - 30) 0
    uncurry (centerShowText $ show (down key minor_third) ++ "m") $ point (cx - 75) 0

-- | For a list of keys, extract the chords and return all corresponding
-- keys in the Circle of Fifths. The keys of all major chords are added
-- and the keys of all minor chords are transposed by a minor third. You
-- can verify this by looking at a Circle of Fifths representation.
majorChordKeys :: [Key] -> [Key]
majorChordKeys keys = map fst (filter isMajor $ matchingChords_ keys)
            where isMajor (_,major) = major == Just True

minorChordKeys :: [Key] -> [Key]
minorChordKeys keys = map fst (filter isMinor $ matchingChords_ keys)
            where isMinor (_,major) = major == Just False

-- | Render Circle of Fifths
renderCOF colorfunc (w, h) = do
    let cx = realToFrac w / 2
        cy = realToFrac h / 2
    C.setLineCap C.LineCapRound
    C.setLineJoin C.LineJoinRound
    C.setLineWidth 2
    foreach circleOfFifths $ \ key -> renderArc cx cy key colorfunc

-}
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
        do drawKey (realToFrac x * keysize) 0 keysize h (colorfunc $ wr !! x)

    foreach ([0 .. 1] :: [Int]) $ \ x ->
        do let shiftx = keysize / 2 + (keysize - keysize / 1.3) / 2
           drawKey (realToFrac x * keysize + shiftx) 0 (keysize / 1.3) blackheight (colorfunc $ br1 !! x)

    foreach ([0 .. 2] :: [Int]) $ \ x ->
        do let shiftx = keysize * 3.5 + (keysize - keysize / 1.3) / 2
           drawKey (realToFrac x * keysize + shiftx) 0 (keysize / 1.3) blackheight (colorfunc $ br2 !! x)



renderCanvas st (w, h) = do
    C.setFontSize 15
    C.setSourceRGBA 0 0 0 1.0
    C.moveTo 10 250;    C.showText $ "Currently hit notes: "
    C.moveTo 240 250;   C.showText $ niceList 12 (map show st)
    C.moveTo 10 270;    C.showText $ "Chord with only these notes: "
    C.moveTo 240 270;   C.showText $ niceList 1 (map show (chordsWithExactNotes st))
    C.moveTo 10 290;    C.showText $ "All chords with these notes:"
    C.moveTo 240 290;   C.showText $ niceList 5 (map show (chordsWithNotes st))
    C.translate 0 0;    renderKeyboard (grayKeyboard st) (300, 200)
    --C.translate 300 0;  renderCOF (rainbowCOF st) (300, 300)
    where niceList n lst = concat (L.intersperse " " (take n lst))

