import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Maybe

type Interval = Int
type ScaleIntervals = [Interval]
type ChordIntervals = [Interval]

data Key = C | Db | D | Eb | E | F | Gb | G | Ab | A | Bb | B 
    deriving (Show, Eq, Ord, Enum) -- Show,Read,Eq

data Notes = Scale Key ScaleIntervals | Chord Key ChordIntervals | Note Key

-- Intervals
unison          = 0 :: Interval
semitone        = 1 :: Interval
wholetone       = 2 :: Interval
minor_third     = 3 :: Interval
major_third     = 4 :: Interval
perfect_fourth  = 5 :: Interval
tritone         = 6 :: Interval
perfect_fifth   = 7 :: Interval
minor_sixth     = 8 :: Interval
major_sixth     = 9 :: Interval
minor_seventh   = 10 :: Interval
major_seventh   = 11 :: Interval
octave          = 12 :: Interval

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
scale key mode = Scale key (Maybe.fromJust $ M.lookup mode scales)

-- Chords
chords :: M.Map String ChordIntervals
chords = M.fromList [
    ("maj",     [unison, major_third, perfect_fifth]),
    ("dom7",    [unison, major_third, perfect_fifth, minor_seventh]),
    ("maj7",    [unison, major_third, perfect_fifth, major_seventh]),
    ("m",       [unison, minor_third, perfect_fifth]), 
    ("m7",      [unison, minor_third, perfect_fifth, minor_seventh]),
    ("m6",      [unison, minor_third, perfect_fifth, minor_sixth])
    ]
chord key ch = Chord key (Maybe.fromJust $ M.lookup ch chords)

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
keys (Note key) = S.fromList [key]

--cmaj = Scale C major
--cmin = Scale C minor

-- c = foldl addelem [C] [1,1,1,1] where addelem k i = (jump i $ head k) : k
-- d = foldl (\k i -> (jump i $ head k) : k) [C] [1,wholetone,1,1] 
--

-- S.difference (keys (Scale C major)) (keys (Scale A minor))

--  |CC|0|D|1|EE|FF|2|G|3|A|4|BB
--  |CC|0|D|1|EE|FF|2|G|3|A|4|BB
--  |CCC|DDD|EEE|FFF|GGG|AAA|BBB

prettyKeys :: S.Set(Key) -> [Char]
prettyKeys k = concat ["|", linetop, "|\n|", linetop, "|\n|", linetop, "|\n|", lineend, "|\n"]
    where
        clean arr = concat (L.intersperse "|" $ filter (\x -> length x > 0) arr)
        linetop = clean $ bloat [2,1,1,1,2,2,1,1,1,1,1,2] combo
        lineend = clean $ bloat [3,0,3,0,3,3,0,3,0,3,0,3] combo
        combo = map (\x -> if (S.member x k) then '#' else ' ') [C .. B]
        bloat p l = map (\(num,cha) -> take num $ repeat cha) $ zip p l

-- replace "0" "*" ascii_keyboard
--    where
--        ascii_keyboard = "|00|1|2|3|44|55|6|7|8|9|A|BB|\n"

ppkeys k = putStr $ prettyKeys (keys k)
main = ppkeys (scale C "major")


--isDiatonicSacle :: Scale -> Bool



--intervals :: Scale -> ScaleIntervals
--intervals = 


-- In music, a whole tone scale is a scale in which each note is separated from its neighbors by the interval of a whole step
-- isWholeToneScale :: ScaleIntervals -> Bool
-- isWholeToneScale s = all (== wholetone) s



--------

-- type Interval' = Key -> Key
--instance Eq Interval' where
--    x == y = x C == y C

-- unison' = id :: Interval'
--st = succ :: Interval'
--wt = st . st :: Interval'

--major' = [st,st,wt,wt
--scale :: Key -> ScaleIntervals -> Scale
--scale k si = ...
