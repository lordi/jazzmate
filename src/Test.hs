
module Main where

import Test.QuickCheck
import MusicTheory
import System.Exit (exitFailure)

instance Arbitrary Note where
    arbitrary = elements ([C .. ])

instance Arbitrary ChordType where
    arbitrary = elements ([Major .. ])

instance Arbitrary ScaleType where
    arbitrary = elements ([MajorPentatonic .. ])

testNoteShow :: Note -> Bool
testNoteShow x = length (show x) > 0

testChordTypeShow :: ChordType -> Bool
testChordTypeShow x = length (show x) >= 0

main = do
    quickCheck testNoteShow
    quickCheck testChordTypeShow
