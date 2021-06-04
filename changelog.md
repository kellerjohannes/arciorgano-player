## changes for version 1.1 (1st of February 2021)

- sampler: increased polyphony management to 128 simultaneous voices
- sampler: added level value display
- sampler: added mute toggle
- sampler: bug fix, introduced 20ms fade-in and fade-out for the sample playback to avoid clicking noises
- sampler: bug fix, introduced 20ms ramp when changing the level value, to reduce distortion while changing the level
- pedal: while pedal on, notes can be 'clicked off' manually
- bug fix: removed non-functional local recording feature on the listener side, which caused an error message when loading the patch
- bug fix: improved reliability of the 'all notes off' module, by introducing a delay ('spread' in ms) between each 'key off' message
- MIDI module: default values for split velocity changed to 53


## changes for version 1.15 (4th of February 2021)

- sampler: introduced a function to avoid redundant key-on messages. This removes the bug that caused multiple playback of the same sample when connected to a MIDI keyboard while using the pedal function


## changes for version 1.20 (5th of February 2021)

- added feature: sine tone generator, with 16 oscillators and 6 input slots. Each input slot can hold a text file that defines the frequency, level and on/off of each oscillator.


## changes for version 1.30 (8th of February 2021)

- added feature: Sibelius bridge. This feature requires a Sibelius plugin that interprets a notation for Arciorgano and translates it into PitchBend-commands. When playing back a score within Sibelius, it sends out MIDI signals that can be caught by this Pd patch and sent to the organ (or the internal sampler) in real time.


## changes for version 1.31 (15th of February 2021)

- added samples quick load for mode 1, 2, 3, 7 and 8.


## changes for version 1.32 (17th of February 2021)

- bug fix in sampler: key number 9 was ignored by the redundancy module, this error is now corrected.

## changes for version 1.33 (15th of April 2021)

- added Reaper bridge: it listenes to incoming MIDI messages and translates them to organ messages. On channel one, the keys on the lower manual are numbered from 0 to 75, on channel two, the keys of the upper manual are numberes from 0 to 61.

## changes for version 1.34 (10th of May 2021)

- added serial communication for direct connection between computer (running the Pd patch) and the organ module. This is intended to be an alternative to OSC messages, primarily for concert situations.

## changes for version 1.40 (10th of May 2021)

- rearranged GUI, no added features, no bug fixes

## changes for version 1.41 (13th of May 2021)

- added "Velocity bridge", to read MIDI information where alterations are encoded in velocity values of NoteOn AND NoteOff messages. 

## changes for version 1.42 (14th of May 2021)

- new feature: ear training quiz for Vicentino's intervals in mode1, as a subpatch in a detached window.

## changes for version 1.43 (31th of May 2021)

- added more intervals and functionality to the ear training quiz subpatch.



## changes for version 1.44 [no date yet]

- added support for pressing the 'p'-key to trigger panic (all notes off)

- adapted the mapping for MIDI values to suit a piece by Anna Sowa


