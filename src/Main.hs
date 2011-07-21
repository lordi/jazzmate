module Main where

import qualified GUI.Main as G
import qualified MIDI.JackBridge as Bridge

main = G.main Bridge.run
