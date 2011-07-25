module GUI.Render where

import qualified Graphics.Rendering.Cairo as C
import qualified Data.Maybe as M
import qualified Data.List as L

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV

import Core

foreach :: (Monad m) => [a] -> (a -> m b) -> m [b]
foreach = flip mapM

-- Circle of Fifths stuff:
keyToCOFAngle :: Key -> Double
keyToCOFAngle key = fromIntegral (M.fromJust $ L.elemIndex key circleOfFifths) * 30.0

keyToCOFColor key True hl = hsv (keyToCOFAngle key) (if hl then 1.0 else 0.25) 0.95
keyToCOFColor key False hl = hsv (keyToCOFAngle (up key minor_third)) (if hl then 1.0 else 0.25) 0.8

renderArc :: Double     -- Center x coordinate
            -> Double   -- Center y coordinate
            -> Key      -- Key in the Circle of Fifths
            -> Bool     -- If True, highlight the major part of the arc
            -> Bool     -- If True, highlight the minor part of the arc
            -> C.Render ()
renderArc cx cy key hlmaj hlmin = do
    let radians = (keyToCOFAngle key) * pi / 180.0 :: Double
        radiansL = (radians - (pi / 2) - 15 * pi / 180.0)
        radiansR = (radians - (pi / 2) + 15 * pi / 180.0)
        RGB majr majg majb = keyToCOFColor key True hlmaj
        RGB minr ming minb = keyToCOFColor (down key minor_third) False hlmin
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
    uncurry C.moveTo $ point (cx - 30) 0
    C.showText $ show key
    uncurry C.moveTo $ point (cx - 75) 0
    C.showText $ (show (down key minor_third)) ++ "m"

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
renderCircleOfFifths keys (w, h) = do
    let cx = realToFrac w / 2
        cy = realToFrac h / 2
        hlmaj = flip elem (majorChordKeys keys)
        hlmin = flip elem (minorChordKeys keys)
    C.setLineCap C.LineCapRound
    C.setLineJoin C.LineJoinRound
    C.setLineWidth 2
    foreach circleOfFifths $ \ key -> renderArc cx cy key (hlmaj key) (hlmin (down key minor_third))

-- | First try to write a function that renders a piano on the screen. Still
-- very ugly duckling, needs rewrite.
renderKeyboard keys (w, h) = do
    let keysize = realToFrac w / 7
        blackheight = realToFrac h * 2 / 3
        br1 = [Db,Eb]
        br2 = [Gb,Ab,Bb]
        wr = [C,D,E,F,G,A,B]
        color _ ((key,Just isMajor):_) = keyToCOFColor key isMajor True
        color key _ = (RGB 0.5 0.5 0.5)
        whitecolor key = if (key `elem` keys) then (color key $ matchingChords_ keys) else (RGB 1.0 1.0 1.0)
        blackcolor key = if (key `elem` keys) then (color key $ matchingChords_ keys) else (RGB 0.0 0.0 0.0)
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
        do let col = whitecolor $ wr !! x
           drawKey (realToFrac x * keysize) 0 keysize h col

    foreach ([0 .. 1] :: [Int]) $ \ x ->
        do let col = blackcolor $ br1 !! x
               shiftx = keysize / 2 + (keysize - keysize / 1.3) / 2
           drawKey (realToFrac x * keysize + shiftx) 0 (keysize / 1.3) blackheight col

    foreach ([0 .. 2] :: [Int]) $ \ x ->
        do let col = blackcolor $ br2 !! x
               shiftx = keysize * 3.5 + (keysize - keysize / 1.3) / 2
           drawKey (realToFrac x * keysize + shiftx) 0 (keysize / 1.3) blackheight col


renderCanvas st (w, h) = do
    C.setSourceRGBA 0 0 0 1.0
    C.moveTo 10 250; C.showText $ "Currently pressed keys: "
    C.moveTo 150 250; C.showText $ show st
    C.moveTo 10 270; C.showText $ "Matching chords: "
    C.moveTo 150 270; C.showText $ show (matchingChords st)
    renderKeyboard st (300, 200)
    C.translate 300 0
    renderCircleOfFifths st (300, 300)

