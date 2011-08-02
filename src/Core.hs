module Core (module MusicTheory, blackNotes, chordsWithNotes,
    chordsWithExactNotes, toPitch, fromPitch, noteToCOFAngle) where

import qualified Data.Maybe as M
import qualified Data.List as L
import qualified Sound.MIDI.Message as Msg
import Sound.MIDI.Message.Channel (T (Cons), messageBody, Body (Voice))
import qualified Sound.MIDI.Message.Channel.Voice as V

import MusicTheory

blackNotes = [C',D',F',G',A']

noteToCOFAngle n = fromIntegral (M.fromJust $ L.elemIndex n (circleOfFifths C)) * 30.0

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
        null $ (notes chord) L.\\ ns]

-- | Converts a MIDI pitch to a Note as defined in MusicTheory
fromPitch :: V.Pitch -> Note
fromPitch p = toEnum ((V.fromPitch p) `mod` (fromEnum PerfectOctave))-- octave)

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



data Notes = Scale Key ScaleIntervals | Chord Key ChordIntervals | Notes [Key]

instance Eq Notes where
    x == y = (keys x) == (keys y)

scale key mode = Scale key (fromJust $ M.lookup mode scales)
chord key ch = Chord key (fromJust $ M.lookup ch chords)

-- The following functions define cyclic movements on the keys
semitone_up :: Key -> Key
semitone_up B = C
semitone_up k = succ k

semitone_down :: Key -> Key
semitone_down C = B
semitone_down k = pred k

up :: Key -> Interval -> Key
up k i = (iterate semitone_up k) !! i

down :: Key -> Interval -> Key
down k i = (iterate semitone_down k) !! i

keys :: Notes -> S.Set(Key)
keys (Scale key is) = S.fromList keylist
    where keylist = foldl (\k i -> (head k `up` i) : k) [key] is
keys (Chord key is) = S.fromList $ map (up key) is
keys (Notes k) = S.fromList k

interval :: Key -> Key -> Interval
interval a b = ((fromEnum a) - (fromEnum b)) `mod` octave

intervals :: [Key] -> ScaleIntervals
intervals [] = []
intervals (x:[]) = []
intervals (x:xs) = (interval x (head xs) : (intervals xs))

-- *Core> (chord C "maj") == (Notes [C, G, E])
-- True
-- *Core> (chord C "maj") == (Notes [C, G, E, F])
-- False
--
-- In music, a whole tone scale is a scale in which each note is separated from its neighbors by the interval of a whole step
-- isWholeToneScale :: ScaleIntervals -> Bool
-- isWholeToneScale s = all (== wholetone) s

-- | For a given list of pressed keys, return a list like [(C, ["maj7"])]
-- FIXME: too ugly/complicated?
matchingChords :: [Key] -> [(Key,[String])]
matchingChords n = filter (not . empty2) $ map (\x -> (x, matches x)) n
            where
                matches x = M.keys $ M.filter (== (Notes n)) $ M.map (Chord x) chords
                empty2 k = null $ snd k

-- | Alternative version: Return a list like [(C, True)]
matchingChords_ keys = map s (matchingChords keys)
            where s (k, modes)
                        | "maj" `elem` modes || "maj7" `elem` modes || "7" `elem` modes = (k, Just True)
                        | "m" `elem` modes || "m7" `elem` modes     = (k, Just False)
                        | otherwise                                 = (k, Nothing)



-- Circle of Fifths stuff:
circleOfFifths :: [Key]
circleOfFifths = take 12 $ iterate (flip up $ perfect_fifth) C

keyToCOFAngle :: Key -> Double
keyToCOFAngle key = fromIntegral (fromJust $ L.elemIndex key circleOfFifths) * 30.0



type MyState = [Key]

-- | The core of this module's functionality: Take a MIDI message and a list
-- of keys, and return the resulting list. Pressing a key will add the
-- corresponding note to the list, lifting it will delete it.
transform :: Msg.T -> [Key] -> [Key]
transform (Msg.Channel Cons {messageBody = (Voice (V.KeyOn p _))})
            = (:) $ fromPitch p
transform (Msg.Channel Cons {messageBody = (Voice (V.KeyOff p _))})
            = L.delete $ fromPitch p
transform _ = id
-}
