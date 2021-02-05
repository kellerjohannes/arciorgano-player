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

