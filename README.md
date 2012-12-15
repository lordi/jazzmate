JazzMate - Live Scale and Chord Utility for Piano Players
=========================================================

For a given combination of pressed notes on a MIDI keyboard, jazzmate will
print out all matching chords' names, as well as a Cairo rendering of a keyboard layout, highlighting the keys in question. Furthermore it displays the [circle of fifths](http://en.wikipedia.org/wiki/Circle_of_fifths), so that the chords' relationships can be easily identified.

My main objectives to write this program were to learn both musical chord names and Haskell. It is my first Haskell programm so please forgive the stinky code.

Screenshot (work in progress)
---------------------------------

![Screenshot of the development version](/lordi/jazzmate/raw/master/screenshot.png)

~~As you can see in the above screenshot, the application utilizes a synaesthetic approach to help the user learn chords and their relationships: A color wheel is mapped onto the circle of fifths to aid the intuitive learning, and the chord's color is also used on the piano display on the left.~~

Usage
-----

If you are using the ALSA MIDI provider, you can simply start JazzMate by typing:

> jazzmate -d alsa

Then connect your MIDI keyboard to JazzMate via aconnect, patchage or qjackctl and starting jamming!

The current scale
-----------------

TODO

Code structure
--------------

The program is written in Haskell and interfaces with ALSA or the JACK audio
daemon to receive MIDI input. These MIDI clients can be found in the MIDI
folder. MIDI messages will be send to a Control.Concurrent.Chan.

The GUI.Main module is responsible to collect the notes from the Chan and
store them in a state variable. It creates a GTK window and uses Cairo to
render a display of the pressed notes.

Last but not least, the core of the program consists of purely functional code
to juggle with notes, scales and chords. To do that, I incorporate a nice and
clean Haskell module called MusicTheory (also available 
[here](https://music-theory.googlecode.com/)) that implements a great deal of
western music theory. Whenever needed, this is extended in the Core module.

Requirements
------------

 * MIDI keyboard
 * ALSA or JACK Haskell bindings to receive MIDI Input
 * Further Haskell dependencies: midi, gtk, cairo, colour

JACK support
------------

By default, JACK support is not enabled because Hackage only provides the
legacy 0.5 version of the Haskell bindings. As of June 2011, in order to use
JACK, you have to use the darcs version at http://code.haskell.org/jack/.

Then you can configure JazzMate as follows:

> cabal configure -f jack

Links
-----

 * [Impro-Visor](http://www.cs.hmc.edu/~keller/jazz/improvisor/), Open Source
   composition software that also helps you jammin'
