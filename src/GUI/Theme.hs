module GUI.Theme where

import qualified Data.Maybe as M
import qualified Data.List as L

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV

import Core

isBlackKey k = k `elem` [Db,Eb,Gb,Ab,Bb]

-- Boring gray theme
grayKeyboard :: [Key] -> Key -> RGB Double
grayKeyboard pressedKeys k
    | k `elem` pressedKeys  = RGB 0.5 0.5 0.5
    | isBlackKey k          = RGB 0.0 0.0 0.0
    | otherwise             = RGB 1.0 1.0 1.0

grayCOF :: [Key] -> Key -> Bool -> RGB Double
grayCOF pressedKeys key isMajor
    | (key, Just isMajor) `elem` chords  = RGB 0.5 0.5 0.5
    | otherwise                          = RGB 1.0 1.0 1.0
    where
        chords = matchingChords_ pressedKeys

-- Colorful rainbow theme
rainbowKeyboard :: [Key] -> Key -> RGB Double
rainbowKeyboard pressedKeys key
    | key `elem` pressedKeys      = chordsToCOFColor chords
    | isBlackKey key              = RGB 0.0 0.0 0.0
    | otherwise                   = RGB 1.0 1.0 1.0
    where
        chords = matchingChords_ pressedKeys

rainbowCOF :: [Key] -> Key -> Bool -> RGB Double
rainbowCOF pressedKeys key isMajor
    | (key, Just isMajor) `elem` chords     = chordsToCOFColor [(key, Just isMajor)]
    | otherwise                             = unhighlight $ chordsToCOFColor [(key, Just isMajor)]
    where
        chords = matchingChords_ pressedKeys

chordsToCOFColor ((key, Just isMajor):_)
                | isMajor       = hsv (keyToCOFAngle key) 1.0 0.95
                | otherwise     = hsv (keyToCOFAngle (key `up` minor_third)) 1.0 0.8
chordsToCOFColor _ = RGB 0.5 0.5 0.5

unhighlight r = hsv h 0.3 v
    where h = hue r
--          s = saturation r
          v = value r


-- hlmaj = flip elem (majorChordKeys keys)
-- hlmin = flip elem (minorChordKeys keys)
--        color _ ((key,Just isMajor):_) = keyToCOFColor key isMajor True
--        color key _ = (RGB 0.5 0.5 0.5)
--        whitecolor key = if (key `elem` keys) then (color key $ matchingChords_ keys) else (RGB 1.0 1.0 1.0)
--        blackcolor key = if (key `elem` keys) then (color key $ matchingChords_ keys) else (RGB 0.0 0.0 0.0)
----keyToCOFColor key True hl = hsv (keyToCOFAngle key) (if hl then 1.0 else 0.25) 0.95
--keyToCOFColor key False hl = hsv (keyToCOFAngle (up key minor_third)) (if hl then 1.0 else 0.25) 0.8

