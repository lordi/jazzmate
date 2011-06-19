scalehs - Scale and Chord Utility for Piano Players
===================================================

For a given combination of pressed notes on a MIDI keyboard, scalehs will
print out all matching chords, as well as an ugly ASCII representation of the
piano, highlighting the keys in question. I wrote this program to learn both
Haskell and musical chord names.

Example output (work in progress)
---------------------------------

    [G,E,C]
    [3,4]
    [(C,["maj"])]
    |##| | | |##|  | |#| | | |  |
    |##| | | |##|  | |#| | | |  |
    |##| | | |##|  | |#| | | |  |
    |###|   |###|   |###|   |   |
    
    [C,A,Gb,D]
    [3,3,4]
    [(D,["7"])]
    |##| |#| |  |  |#| | |#| |  |
    |##| |#| |  |  |#| | |#| |  |
    |##| |#| |  |  |#| | |#| |  |
    |###|###|   |   |   |###|   |

Code structure
--------------

The program is written in Haskell and interfaces with the JACK audio daemon to
receive MIDI input. The Core module consists of purely functional code to
manipulate scales and retrieve chords. It also contains the mappings from
chord and scale names to the musical intervals. The Main module is responsible
to collect the notes that are currently pressed. It does that by a State
transformer that transforms a list of keys based on an incoming MIDI message.

Requirements
------------

 * Bindings to the JACK Audio Connection Kit for Haskell (Jack 0.6). As of
   June 2011, you have to use the darcs version at
   http://code.haskell.org/jack/, since Hackage only provides 0.5.

