********************************************************************************
TIAtune v0.1.0
by utz 11'2017 * irrlichtproject.de
********************************************************************************


ABOUT
================================================================================

TIAtune is a music driver for the Atari 2600 (VCS) console. Unlike other TIA
music drivers, it does not rely on the on the TIA's built-in frequency
dividers. That means that the detune usually associated with TIA music is absent
in TIAtune.


Features
========

- 16-bit frequency dividers (accurate pitch within <1% margin)
- sample rate: 10.4 KHz
- per-step tempo, 6-bit tempo resolution


Limitations
===========

- 100% CPU time used, cannot render graphics at the same time as playing music
- fairly large player (1147 bytes)
- no support for AUDC waveforms 2, 3, E


The TIAtune source is designed to be assembled with the ACME cross-assembler.


XM CONVERTER
================================================================================

You can use the provided music.xm template to compose music for TIAtune. To use
the converter, you must first install the ACME assembler, which is available at 
https://sourceforge.net/projects/acme-crossass/. You should also have the Stella
emulator installed. Then simply run compile.cmd (Windows) or compile.sh 
(*nix, Mac OS X) to convert, assemble, and run the result in Stella.

The following limitations apply:

- Any changes to instruments/samples are ignored.
- Changes to BPM are ignored.
- FX commands are ignored, except for Fxx (change tempo, xx < 0x20)
- The range of instrument 3 is limited to C-0..E-4, and the range of instruments
  4 and 5 is limited to C-0..E-3.

Pattern length is also limited. The actual limit depends on the converted data
size. In the worst case, you will run out of bytes after 51 rows, but normally
you can get away with 64 and more rows. Generally it is a good idea to keep
patterns short, though. The converter optimizes pattern data, but not the 
overall pattern structure. That means you can often save some bytes by breaking 
down your patterns into smaller parts and removing redundancies.

Linux and Mac users will have to build the converter from source. Provided you 
have Rust and Cargo installed, building the converter should be a simple matter
of running

  cargo build --release

in the xm2tiatune directory and then moving the build from target/release to the
main directory.


MUSIC DATA FORMAT
================================================================================

Music data for TIAtune consists of a sequence, followed by one or more patterns.
The music data must be provided in a file named "music.asm", which is included
by the main file.


Sequence
========

The sequence contains a list of pointers to patterns, in the order in which they
are to be played. Is is split into a hi-byte and a lo-byte part, labelled 
"sequence_hi" and "sequence_lo", respectively. The hi-byte list must be
terminated with a 0-byte.

The sequence may at most contain 255 entries. The most simple sequence 
would thus be:

sequence_hi
	!byte pattern>>8
	!byte 0
sequence_lo
	!byte pattern&$ff
	

Patterns
========

Patterns contain the actual music data. They consist of one or more rows
(steps), which in turn consist of 1-5 data bytes. The function of the data bytes
is as follows:

byte  bits   function
1     0..5   tempo (step length)
      6      if set, skip updating channel 2
      7      if set, skip updating channel 1
2     0..1   waveform channel 1 (0..4)
      2..5   volume ch1
3     0..7   note ch1
4     0..1   waveform ch2 (0..4)
      2..5   volume ch2
5     0..8   note ch2

If bit 7 of byte 1 is set, byte 2 and 3 are omitted. Likewise, if bit 6 of byte
1 is set, byte 4 and 5 are omitted. On the first step of the first pattern in
the sequence, no data bytes may be omitted.

AUDCx equivalents of the waveform parameter, and their note ranges are as 
follows:

wave  AUDCx      range
0     4,5,C,D    c-0..dis8
1     8          c-0..dis8
2     1	         c-0..e-4
3     6,A        c-0..e-3
4     7,9        c-0..e-3

The remaining AUDCx waveforms are not supported by TIAtune.

Each pattern may contain up to 255 data bytes. Thus, each pattern may contain at
least 51 steps. In most cases however, it is advisable to use shorter patterns,
to optimize overall data usage.

Each pattern must be terminated with a 0-byte.

For more information, check the provided example music.asm file.
