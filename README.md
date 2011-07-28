scalehs - Scale and Chord Utility for Piano Players
===================================================

For a given combination of pressed notes on a MIDI keyboard, scalehs will
print out all matching chords' names, as well as a Cairo rendering of a keyboard layout, highlighting the keys in question. Furthermore it displays the [circle of fifths](http://en.wikipedia.org/wiki/Circle_of_fifths), so that the chords' relationships can be easily identified.

My main objectives to write this program were to learn both musical chord names and Haskell. It is my first Haskell programm so please forgive the stinky code.

Screenshot (work in progress)
---------------------------------

![Screenshot of the development version](/lordi/scalehs/raw/master/screenshot.png)

As you can see in the above screenshot, the application utilizes a synaesthetic approach to help the user learn chords and their relationships: A color wheel is mapped onto the circle of fifths to aid the intuitive learning, and the chord's color is also used on the piano display on the left.

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

 * MIDI keyboard
 * Bindings to the JACK Audio Connection Kit for Haskell (Jack 0.6). As of
   June 2011, you have to use the darcs version at
   http://code.haskell.org/jack/, since Hackage only provides 0.5.
 * Further Haskell dependencies: midi, gtk, cairo, colour
