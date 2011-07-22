module Main where

import qualified GUI.Main as GUI
import qualified MIDI.JackBridge as Jack
import qualified MIDI.Dummy as Dummy

main = GUI.main Dummy.run
