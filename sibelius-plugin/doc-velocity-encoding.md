# Velocity encoding

Sibelius note names are based on these accidentals:
- normal sharp: #
- normal flat: b
- quarter sharp: +
- three-quarter sharp: #+
- quarter flat: -
- three-quarter flat: b-


Velocity 99
E# B#

Velocity 100
C C# D D# E F F# G G# A A# B

Velocity 101
C#+ D#+ F#+ G#+ A#+ C+ D+ E+ F+ G+ A+ B+ Db- Eb- Gb- Ab- Bb-

Velocity 102
Fb Cb Db Eb Gb Ab Bb

Velocity 103
D- E- G- A- B-


To calculate the Arciorgano key number do the following:
- Subtract 100 from the velocity, this is Delta.
- Look up the key number of the Midi Pitch of the note.
- Add Delta to this key number.