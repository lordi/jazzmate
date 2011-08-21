Some further ideas to implement
===============================

* ~~Print out nice unicode key representation instead of "Eb" for instance~~
* ~~Center align texts in circle of fifths~~
* ~~Bigger current chord display~~
* ~~"potentials": color other keys from the keyboard in the color as if they
  would have been pressed too (probably only that of neighbours)~~ dismissed,
  this is not really working that well
* ~~MIDI through ALSA connection~~
* ~~make jack dependency optional~~
* Dynamic size
* ability to specify split key
* break up render functions into own .hs files
* ~~ability to give the render function custom color functions so that the
  coloring can be turned of easily (in case its annoying)~~
* ~~clean up Core.hs (maybe use https://music-theory.googlecode.com/svn-history/r18/trunk/MusicTheory.hs which appears to be very similar yet much cleaner)~~
* ~~GTK dropdown box for MIDI input / split key~~ dismissed, by design the programs only input should be midi [really? maybe also add buttons to select key and mode]
* ~~add chromatic, whole tone, melodic and harmonic minor scales to MusicTheory~~
* ~~add Show instance for Scale~~
* Can the current key and mode/scale be guessed solidly by the last n notes that were played?
* Bug: Sometimes JazzMate freezes and does not update the main window anymore
* Add scale degrees to Circle of fifths
