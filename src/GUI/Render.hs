module GUI.Render where

import qualified Graphics.Rendering.Cairo as C
import qualified Data.Maybe as M
import qualified Data.List as L
import Core

foreach :: (Monad m) => [a] -> (a -> m b) -> m [b]
foreach = flip mapM

keyToAngle :: Key -> Double
keyToAngle key = fromIntegral (M.fromJust $ L.elemIndex key circleOfFifths) * 30.0

renderArc :: Double -> Double -> Bool -> Key -> C.Render ()
renderArc cx cy highlight key = do
    let radians = (keyToAngle key) * pi / 180.0 :: Double
        point dist gamma = (x, y)
            where x = cx + (sin (radians + gamma)) * dist
                  y = cy - (cos (radians + gamma)) * dist

    uncurry C.moveTo $ point 30 (-0.2)
    uncurry C.lineTo $ point (cx - 10) (-0.2)
    uncurry C.lineTo $ point (cx - 10) 0.2
    uncurry C.lineTo $ point 30 0.2
    uncurry C.lineTo $ point 30 (-0.2)
    C.closePath
    C.setSourceRGBA 1.0 1.0 (if highlight then 1.0 else 0.0) 1.0
    C.fillPreserve
    C.setSourceRGBA 0.0 0.0 0.0 1.0
    C.stroke
    uncurry C.moveTo $ point (cx - 30) 0
    C.showText $ show key


renderCircleOfFifths keys (w, h) = do
    let cx = realToFrac w / 2
        cy = realToFrac h / 2
        onlyMaj (_, mode) = "maj" `elem` mode || "maj7" `elem` mode
        highlight key = key `elem` (map fst (filter onlyMaj (matchingChords keys)))
    foreach circleOfFifths $ \ key -> renderArc cx cy (highlight key) key
    --renderArc cx cy True C


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
    C.translate 300 0
    renderCircleOfFifths st (300, 300)


