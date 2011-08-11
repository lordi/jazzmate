module Core (module MusicTheory, blackNotes, chordsWithNotes,
    chordsWithExactNotes, toPitch, fromPitch, noteToCOFAngle,
    scalesWithNotes, histogram) where

import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M
import qualified Sound.MIDI.Message as Msg
import Sound.MIDI.Message.Channel (T (Cons), messageBody, Body (Voice))
import qualified Sound.MIDI.Message.Channel.Voice as V

import MusicTheory

blackNotes = [C',D',F',G',A']

noteToCOFAngle n = fromIntegral (fromJust $ L.elemIndex n (circleOfFifths C)) * 30.0

chordsWithNotes :: [Note] -> [Chord]
chordsWithNotes [] = []
chordsWithNotes ns =
    [ chord |
        n <- [minBound..],
        t <- [minBound..],
        let chord = Chord n t,
        all (inChord chord) ns]

chordsWithExactNotes :: [Note] -> [Chord]
chordsWithExactNotes ns =
    [ chord |
        chord <- chordsWithNotes ns,
        null $ notes chord L.\\ ns]

scalesWithNotes :: [Note] -> [Scale]
scalesWithNotes ns =
    [ scale |
        n <- [minBound..],
        t <- [minBound..],
        let scale = Scale n t,
        all (`elem` notes scale) ns && null (notes scale L.\\ ns)]

histogram :: Ord a => [a] -> M.Map a Int
histogram xs = M.fromList [ (head l, length l) | l <- L.group (L.sort xs) ]

-- | Converts a MIDI pitch to a Note as defined in MusicTheory
fromPitch :: V.Pitch -> Note
fromPitch p = toEnum (V.fromPitch p `mod` fromEnum PerfectOctave)

toPitch :: Note -> V.Pitch
toPitch k = V.toPitch (fromEnum k)



{-
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe

import qualified Sound.MIDI.Message as Msg
import Sound.MIDI.Message.Channel (T (Cons), messageBody, Body (Voice))
import qualified Sound.MIDI.Message.Channel.Voice as V

type Interval = Int
type ScaleIntervals = [Interval]
type ChordIntervals = [Interval]

data Key = C | Db | D | Eb | E | F | Gb | G | Ab | A | Bb | B
    deriving (Show, Eq, Ord, Enum)

-- Intervals
unison          = 0 :: Interval
semitone        = 1 :: Interval
minor_second    = 1 :: Interval
wholetone       = 2 :: Interval
major_second    = 2 :: Interval
minor_third     = 3 :: Interval
major_third     = 4 :: Interval
perfect_fourth  = 5 :: Interval
tritone         = 6 :: Interval
raised_fourth   = 6 :: Interval
perfect_fifth   = 7 :: Interval
minor_sixth     = 8 :: Interval
major_sixth     = 9 :: Interval
minor_seventh   = 10 :: Interval
major_seventh   = 11 :: Interval
octave          = 12 :: Interval
nineth          = 13 :: Interval

-- Modes
scales :: M.Map String ScaleIntervals
scales = M.fromList [
    ("major",       [2,2,1,2,2,2,1]),
    ("dorian",      [2,1,2,2,2,1,2]),
    ("phrygian",    [1,2,2,2,1,2,2]),
    ("minor",       [2,1,2,2,1,2,2]),
    ("blues",       [3,2,1,1,3,2]),
    ("chromatic",   take 12 $ repeat semitone),
    ("whole tone",  take 6 $ repeat wholetone)
    ]

-- Chords
chords :: M.Map String ChordIntervals
chords = M.fromList [
    ("maj",     [unison, major_third, perfect_fifth]),
    ("6",       [unison, major_third, perfect_fifth, major_sixth]),
    ("7",       [unison, major_third, perfect_fifth, minor_seventh]),
    ("maj7",    [unison, major_third, perfect_fifth, major_seventh]),
    ("m",       [unison, minor_third, perfect_fifth]), 
    ("m6",      [unison, minor_third, perfect_fifth, major_sixth]),
    ("m7",      [unison, minor_third, perfect_fifth, minor_seventh]),
    ("m9",      [unison, minor_third, minor_seventh, 14]),
    ("m9'",     [unison, minor_third, perfect_fifth, minor_seventh, 14]),
    ("dim",     [unison, minor_third, tritone]),
    ("dim7",    [unison, minor_third, tritone, major_sixth]),
    ("add9",    [unison, major_third, perfect_fifth, 14]),
    ("aug",     [unison, major_third, minor_sixth]),
    ("sus2",    [unison, wholetone, perfect_fifth]),
    ("sus4",    [unison, perfect_fourth, perfect_fifth])
    ]
--}
