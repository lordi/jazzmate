module Main where

import qualified GUI.Main as GUI
--import qualified MIDI.JACK as JACK
import qualified MIDI.ALSA as ALSA
import qualified MIDI.Dummy as Dummy

main = GUI.main ALSA.run
