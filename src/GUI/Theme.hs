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
    | (key, Just isMajor) `elem` chords = chordsToCOFColor [(key, Just isMajor)]
    | otherwise                         = dampen $ chordsToCOFColor [(key, Just isMajor)]
    where
        chords = matchingChords_ pressedKeys
        dampen r = hsv (hue r) 0.25 (value r)

chordsToCOFColor ((key, Just isMajor):_)
                | isMajor       = hsv (keyToCOFAngle key) 1.0 0.95
                | otherwise     = hsv (keyToCOFAngle (key `up` minor_third)) 1.0 0.8
chordsToCOFColor _ = RGB 0.5 0.5 0.5

