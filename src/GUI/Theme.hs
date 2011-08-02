module GUI.Theme where

import qualified Data.List as L

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV

import Core

majorChordTypes = [Major, Major7th, Dominant7th]
minorChordTypes = [Minor, Minor7th]

-- Boring gray theme
grayKeyboard :: [Note] -> Note -> RGB Double
grayKeyboard pressedNotes n
    | n `elem` pressedNotes = RGB 0.5 0.5 0.5
    | n `elem` blackNotes   = RGB 0.0 0.0 0.0
    | otherwise             = RGB 1.0 1.0 1.0

grayCOF :: [Note] -> Note -> Bool -> RGB Double
grayCOF pressedNotes note isMajor
    | any (`elem` chords) reprchords    = RGB 0.5 0.5 0.5
    | otherwise                         = RGB 1.0 1.0 1.0
    where
        chords = chordsWithExactNotes pressedNotes
        reprchords = map (\ch -> (Chord note ch)) types
        types = if isMajor then majorChordTypes else minorChordTypes

-- Colorful rainbow theme
{-
rainbowNoteboard :: [Note] -> Note -> RGB Double
rainbowNoteboard pressedNotes n
    | n `elem` pressedNotes = chordsToCOFColor chords
    | n `elem` blackNotes   = RGB 0.0 0.0 0.0
    | otherwise             = RGB 1.0 1.0 1.0
    where
        chords = chordsWithNotes pressedNotes

rainbowCOF :: [Note] -> Note -> Bool -> RGB Double
rainbowCOF pressedNotes note isMajor
    | (note, Just isMajor) `elem` chords    = chordsToCOFColor [(note, Just isMajor)]
    | otherwise                             = dampen $ chordsToCOFColor [(note, Just isMajor)]
    where
        chords = chordsWithNotes pressedNotes
        dampen r = hsv (hue r) 0.25 (value r)

chordsToCOFColor ((note, Just isMajor):_)
                | isMajor       = hsv (noteToCOFAngle note) 1.0 0.95
                | otherwise     = hsv (noteToCOFAngle (note `up` minor_third)) 1.0 0.8
chordsToCOFColor _ = RGB 0.5 0.5 0.5
-}
