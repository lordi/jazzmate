module GUI.Theme where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV

import Core

type Color = RGB Double

yellow = RGB 0.9 0.9 0.2
black = RGB 0.0 0.0 0.0
white = RGB 1.0 1.0 1.0
lightBlue =  RGB 0.75 0.75 0.85
lighterBlue = RGB 0.85 0.85 0.95

blueAndYellowPressedNotes = mkKeyboardColoring yellow
blueAndYellowCOF = mkCOFColoring yellow lighterBlue lightBlue
blueAndYellowScaleNotes = mkKeyboardColoring lightBlue

-- Boring gray theme
mkKeyboardColoring :: Color 
                    -> [Note] -> Note -> Color
mkKeyboardColoring pressedColor pressedNotes n
    | n `elem` pressedNotes = pressedColor
    | n `elem` blackNotes   = black
    | otherwise             = white

mkCOFColoring :: Color -> Color -> Color
                -> [Note] -> Maybe Scale -> Note -> Bool -> Color
mkCOFColoring currentChordColor resolvingChordColor chordInScaleColor
                pressedNotes currentScale note isMajor
    | any (`elem` chords) reprchords            = currentChordColor
    | any (`elem` resolvingChords) reprchords   = resolvingChordColor
    | isJust currentScale && any (inScale (fromJust currentScale)) reprchords = chordInScaleColor
    | otherwise                                 = white
    where
        chords = chordsWithExactNotes pressedNotes
        chord = listToMaybe chords
        reprchords = map (\ch -> (Chord note ch)) types
        types = if isMajor then majorChordTypes else minorChordTypes
        resolvingChords = maybe [] (\scale -> maybe [] ((flip resolvesChord) scale) chord) currentScale


