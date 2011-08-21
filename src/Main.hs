module Main where

import qualified GUI.Main as GUI
import qualified MIDI.Dummy as Dummy
import GUI.Render (renderCanvas, )
import Core

#ifdef USE_JACK
import qualified MIDI.JACK as JACK
#endif
#ifdef USE_ALSA
import qualified MIDI.ALSA as ALSA
#endif

#ifdef USE_ALSA
main = GUI.main ALSA.run renderFunc
#else
#ifdef USE_JACK
main = GUI.main JACK.run renderFunc
#else
main = GUI.main Dummy.run renderFunc
#endif 
#endif
    where renderFunc = renderCanvas (Just $ Scale C MinorDiatonic)
