-- | Western Music Theory
-- Originally from https://music-theory.googlecode.com/
module MusicTheory where

import Data.List
import Data.Maybe
import Monad
import Data.Ord

data Note = C | C' | D | D' | E | F | F' | G | G' | A | A' | B
    deriving (Enum, Bounded, Eq, Ord)

data ScaleDegree = I | II | III | IV | V | VI | VII
    deriving (Enum, Bounded, Eq, Ord)

data ChordType =
      Major
    | Minor
    | SuspendedFourth
    | Major7th
    | Minor7th
    | Dominant7th
    deriving (Bounded, Enum, Eq)

data ScaleType =
      MajorPentatonic
    | MinorPentatonic
    | MajorDiatonic
    | MinorDiatonic -- also known as MinorNatural
    | MinorHarmonic
    | MinorMelodic
    | Chromatic
    | WholeTone
    | Ionian        -- same as MajorDiatonic
    | Dorian
    | Phrygian
    | Lydian
    | Mixolydian
    | Aeolian       -- same as MinorDiatonic
    | Locrian
    deriving (Eq, Bounded, Enum, Show)

data Chord = Chord Note ChordType deriving (Eq)
data Scale = Scale Note ScaleType deriving (Eq)

data DiatonicInterval =
      PerfectUnison
    | MinorSecond
    | MajorSecond
    | MinorThird
    | MajorThird
    | PerfectFourth
    | AugmentedFourth
    | PerfectFifth
    | MinorSixth
    | MajorSixth
    | MinorSeventh
    | MajorSeventh
    | PerfectOctave
    deriving (Eq, Bounded, Enum, Show)

class NoteSet a where
    notes :: a -> [Note]

instance NoteSet Chord where
    notes (Chord root chordType) = root : map (add root) (cIntervals chordType)

instance NoteSet Scale where
    notes (Scale root scaleType) = map (add root) (sIntervals scaleType)

cNotes :: Chord -> [Note]
cNotes c = notes c

sNotes :: Scale -> [Note]
sNotes s = notes s

cIntervals :: ChordType -> [DiatonicInterval]
cIntervals Major = [MajorThird, PerfectFifth]
cIntervals Minor = [MinorThird, PerfectFifth]
cIntervals SuspendedFourth = [PerfectFourth, PerfectFifth]
cIntervals Major7th = cIntervals Major ++ [MajorSeventh]
cIntervals Minor7th = cIntervals Minor ++ [MinorSeventh]
cIntervals Dominant7th = cIntervals Major ++ [MinorSeventh]

sIntervals :: ScaleType -> [DiatonicInterval]
sIntervals MajorPentatonic =
	[PerfectUnison, MajorSecond, MajorThird, PerfectFifth, MajorSixth]
sIntervals MinorPentatonic =
	[PerfectUnison, MinorThird, PerfectFourth, PerfectFifth, MinorSeventh]
sIntervals MajorDiatonic =
	[PerfectUnison, MajorSecond, MajorThird, PerfectFourth, PerfectFifth,
         MajorSixth, MajorSeventh]
sIntervals MinorDiatonic =
	[PerfectUnison, MajorSecond, MinorThird, PerfectFourth, PerfectFifth,
         MinorSixth, MinorSeventh]
sIntervals MinorHarmonic =
	[PerfectUnison, MajorSecond, MinorThird, PerfectFourth, PerfectFifth,
         MinorSixth, MajorSeventh]
sIntervals MinorMelodic =
	[PerfectUnison, MajorSecond, MinorThird, PerfectFourth, PerfectFifth,
         MajorSixth, MajorSeventh]
sIntervals WholeTone = take 6 $ iterate (flip add MajorSecond) PerfectUnison
sIntervals Chromatic = take 12 $ iterate (flip add MinorSecond) PerfectUnison
-- Ionian is the same as MajorDiatonic. The rest of the scales (Dorian to
-- Locrian) can be specified in relation to its predecessor. TODO: verify
sIntervals Ionian = sIntervals MajorDiatonic
sIntervals x = map diminish $ sIntervals (add x (-1))

circleOfFifths :: Note -> [Note]
circleOfFifths note = iterate (flip add PerfectFifth) note

add :: (Eq a, Bounded a, Enum a, Enum b) => a -> b -> a
add x n = toEnum $ (fromEnum x + fromEnum n) `mod`
                   (fromEnum (maxBound `asTypeOf` x)+1)

diminish :: DiatonicInterval -> DiatonicInterval
diminish interval = add interval (-1)
doublyDiminish = diminish . diminish

triadsInScale :: Scale -> [Chord]
triadsInScale scale = filter isTriad (chordsInScale scale)

chordsOfTypeInScale :: Scale -> ChordType -> [Chord]
chordsOfTypeInScale scale chordType =
    filter (\(Chord _ cType) -> cType == chordType) (chordsInScale scale)

chordsInScale :: Scale -> [Chord]
chordsInScale scale =
    [ chord |
      n <- [minBound..],
      t <- [minBound..],
      let chord = Chord n t,
      inScale scale chord]

scalesWithChord :: Chord -> [Scale]
scalesWithChord chord =
    [ scale |
      n <- [minBound..],
      t <- [minBound..],
      let scale = Scale n t,
      inScale scale chord]

chordsWithRoot :: Note -> [Chord]
chordsWithRoot n =
    [chord | t <- [minBound..],
        let chord = Chord n t]

chordsWithNote :: Note -> [Chord]
chordsWithNote note =
    [ chord |
        n <- [minBound..],
        t <- [minBound..],
        let chord = Chord n t,
        inChord chord note]

inScale :: Scale -> Chord -> Bool
inScale scale chord = all (flip elem (notes scale)) (notes chord)

inChord :: Chord -> Note -> Bool
inChord chord note = elem note (notes chord)

scaleSharedChords :: Scale -> Scale -> [Chord]
scaleSharedChords s1 s2 = intersect (chordsInScale s1) (chordsInScale s2)

notesAtScaleDegrees :: [ScaleDegree] -> Scale  -> [Note]
notesAtScaleDegrees degrees scale  = map (((notes scale)!!) . fromEnum) degrees

chordsAtScaleDegrees :: [ScaleDegree] -> Scale ->  [[Chord]]
chordsAtScaleDegrees degrees scale  =
    map (\a -> filter (inScale scale) (chordsWithRoot a) )
        (notesAtScaleDegrees degrees scale)

twelveBarBlues :: Scale -> [[Chord]]
twelveBarBlues scale = chordsAtScaleDegrees [I,I,I,I,IV,IV,I,I,V,V,I,I] scale

--  filter (\a -> elem (toEnum (fromEnum a)) ) (notes scale)
resolvesChord :: Chord -> Scale -> [Chord]
resolvesChord chord@(Chord note chordType) scale = case seventhNote chordType of
    Just seventh ->
        let d7 = add note (diminish seventh)
            dd7 = add note (doublyDiminish seventh)
            resolves c =
                let ns = notes c
                in isTriad c && ((elem d7 ns) || (elem dd7 ns))
        in filter resolves (chordsInScale scale)
    Nothing -> []

seventhNote :: ChordType -> Maybe DiatonicInterval
seventhNote chordType =
    find (\i-> i==MajorSeventh||i==MinorSeventh)
         (cIntervals chordType)

isTriad :: Chord -> Bool
isTriad (Chord _ chordType) = elem chordType [Major, Minor]

-- Notes to self
standardGuitarTuning = [E, A, D, G, B, E]
tetris = [E, B, C, D, C, B, A, A, C, E]
         ++ [D, C, B, B, C, D, E, C, A, A]
         ++ [F, G, A, G, F, E, C, E, D, C, B, E, C, A, A]

-- Read/Show
noteNames = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
chordNames =  ["", "m", "sus4", "maj7", "min7", "7"]

instance Show Note where
    show n = noteNames!!(fromEnum n)

instance Show ChordType where
    show ct = chordNames!!(fromEnum ct)

instance Show Chord where
    show (Chord note chordType) = show note ++ show chordType

instance Show Scale where
    show (Scale note scaleType) = show note ++ " " ++ show scaleType

instance Read Chord where
    readsPrec d s =
        [(Chord n t,x) |
         (n, u) <- readsPrec d s,
         (t, x) <- readsPrec d u]

instance Read Note where
    readsPrec d str = map applyFlats (readEnum noteNames str)
        where applyFlats (n,('b':rest)) = applyFlats (add n (-1), rest)
              applyFlats p = p

instance Read ChordType where
    readsPrec d str = readEnum chordNames str

readEnum :: Enum a => [String] -> String -> [(a, String)]
readEnum candidates str = case longestMatch candidates str of
    Just match -> [(toEnum $ fromJust $ elemIndex match candidates,
                    fromJust $ stripPrefix match str)]
    Nothing -> []

longestMatch :: [String] -> String -> Maybe String
longestMatch candidates str =
    find (`isPrefixOf` str) (sortBy (flip (comparing length)) candidates)

