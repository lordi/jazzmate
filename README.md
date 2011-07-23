scalehs - Scale and Chord Utility for Piano Players
===================================================

For a given combination of pressed notes on a MIDI keyboard, scalehs will
print out all matching chords, as well as ~~an ugly ASCII representation of
the piano~~ a rocking Cairo rendering of a keyboard layout, highlighting the
keys in question. I wrote this program to learn both Haskell and musical chord
names.

Example output (work in progress)
---------------------------------

![Screenshot of the development version](screenshot.jpg "Screenshot")

Code structure
--------------

The program is written in Haskell and interfaces with the JACK audio daemon to
receive MIDI input. This happens in the MIDIBridge module by creating a Chan
and subsequently feeding MIDI messages to it.

The Main module is responsible to collect the notes from the Chan and store
them in a state variable. It creates a GTK window and uses Cairo to render a
display of the pressed notes.

Last but not least, the Core module consists of purely functional code to
manipulate scales and retrieve chords.

Requirements
------------

 * Bindings to the JACK Audio Connection Kit for Haskell (Jack 0.6). As of
   June 2011, you have to use the darcs version at
   http://code.haskell.org/jack/, since Hackage only provides 0.5.
 * Further Haskell dependencies: midi, gtk, cairo
