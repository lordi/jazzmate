module Main where

import qualified GUI.Main as GUI
import qualified MIDI.JackBridge as Jack

main = GUI.main Jack.run
