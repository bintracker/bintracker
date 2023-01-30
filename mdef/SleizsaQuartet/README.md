# Sleizsa Quartet

## Introduction

Sleizsa Quartet is a music player for the [Fairchild Channel F]() gaming console. It renders up to four voices on the console's 1-bit audio output. Software mixing overrides the built-in fixed frequency generator, blocking the CPU entirely in the process.


## Features

- 4 tone channels
- 16-bit frequency dividers
- narrow pulse OR mixing (Squeeker type 1-bit synthesis)
- variable duty cycles and duty cycle sweep
- channel 4 can act as noise channel
- user-definable PWM drums (interrupting tone)
- Sample rate: 29.8 KHz (Drums: 59.6 KHz)


## Usage

### Requirements

- [MAME](https://mamedev.org)
- Channel F ROM files (sl31253.rom, sl31254.rom, sl90025.rom)
- an XM editor, for example [OpenMPT](https://openmpt.org) or [Milkytracker](https://milkytracker.org)

### Making Music

To compose music for Sleizsa Quartet, edit the provided special XM template (music.xm) in an XM editor. Then, run `make` (*nix) or "make.bat" (Windows) to convert music.xm to a Channel F cartridge image that you can then run in an emulator (eg. MAME).

The following restrictions apply when using music.xm:

- You cannot change the BPM, the number of channels, or instrument 1/2.
- You must put tones (instrument 1) in channels 1-4.
- You must put noise in channel 4 only (instrument 2).
- Put drum samples (instrument 3 and up) in channel 5.
- Pitch for drum samples is ignored (always defaults to C-4).
- Instrument and sample settings are ignored.
- Setting the volume sets the duty cycle for the given channel instead
- Most effect commands are ignored, except the following:
  - Axx: interpreted as duty sweep speed
  - ECx: note cut (channel 5/instruments 3 and up only)
  - E5x: fine detune
  - Fxx: tempo only (F01-F1F)
- The sequence loop point works as expected.
- The music.xm template only gives a rough impression of what the track will sound like on an actual Channel F.
- Drum samples interrupt tone and noise playback. By default, drums will play for 2 ticks, or half the current tempo value, whichever is smaller. You can manually change this behavior with the note cut command (ECx). The effective drum duration will be equal to x/2 ticks. x/2 may not be larger than the current value of Fxx.

A number of example drum instruments (4-7) are provided. You can add your own drum instruments. For this, you need to manually generate the required PWM sample data.

### Extended Compilation Options

The following additional targets are available for make/make.bat:

target    | action
----------|---------
run       | build test.bin and run in MAME
write-wav | build test.bin, run in MAME and write sound output to test.wav
debug     | build test.bin and run in MAME in debug mode

All these require MAME to be installed, and the Channel F ROM files present in ./roms/channelf (edit the Makefile resp. make.bat if you want to use a different ROM directory. Sleizsa Quartet does not ship with any ROM files, but they can be easily obtained online. MAME looks for 3 ROM files named  sl31253.rom, sl31254.rom, and sl90025.rom.


## Data Format

Sleizsa Quartet music data follows the common sequence-pattern approach.

### Sequence

The sequence specifies the order in which the patterns should be played. It consists of a list of pattern pointers, followed by a terminating 0-byte, followed by a loop pointer pointing to an address within the sequence.

### Patterns

Patterns consist of one or more rows of music data, followed by a terminating 0-byte. The rows are structured as follows:

offset | function
-------|------------------------------------------------------------
0      | ctrl1 *see below*
1      | lsb, msb frequency divider channel 1
3      | lsb, msb frequency divider ch2
5      | lsb, msb frequency divider ch3
7      | lsb, msb frequency divider ch4
9      | ch4 noise mode (enable: $ff, disable: $00)
10     | ctrl2 *see below*
11     | duty cycle ch1 ($00..$3f)
12     | duty cycle modulation speed ch1 (-128..+127, disable: 0)
13     | duty cycle, duty mod ch2
15     | duty cycle, duty mod ch3
17     | duty cycle, duty mod ch4
18     | ctrl3: *see below*
19     | drum length (negative offset, -[1..[[row_length * 2] - 1])
20     | msb, lsb drum sample pointer
22     | row length

The given offsets are for a full row that sets all possible values. You may skip values that do not need an update on the current row. Configure this behavior by setting the appropriate bits the control bytes.

ctrl | bit | function
-----|-----|-------------------------------------------------------
1    |  0  | load frequency divider ch1
     |  1  | load frequency divider ch2
     |  2  | load frequency divider ch3
     |  3  | load frequency divider, noise mode ch4
     |     | ctrl1 must never be 0. Set any of bit 4..7 if necessary.
2    |  0  | load duty settings ch1
     |  1  | load duty settings ch2
     |  2  | load duty settings ch3
     |  3  | load duty settings ch4
3    |     | set to 0 to enable drum, else omit

### Samples

Sleizsa Quartet uses 1-bit PWM samples for percussion. Each byte in the sample data represents a delay before the next output signal inversion. A data byte of 0 signifies the end of the sample. When converting from PCM WAV (for example with jefftheworld's [pcm2pwm](https://github.com/JeffAlyanak/pcm2pwm) tool), remember that samples play at 59.6 KHz, twice the rate of the tone synthesizer.
