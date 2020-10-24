AMP16K


Description

AMP16K is a ZX Spectrum beeper engine with four tone channels, and a further channel of 
interrupting drums. The tone channels support eight levels of pulse width set in steps of
sound loop iterations (224 T states on the ZX Spectrum 16/48K). Channel mixing is of the
accumulating non-PWM type similar to that used in Squeeker by Zilogat0r. There are eight
predefined drum sounds, with a maximum of 14 allowed by the song data format.

The song tempo is set in steps of 1/50 of a second (the duration of one interrupt on a ZX 
Spectrum) on a per-pattern basis.

As its name suggests, AMP16K supports operation in contended memory, making it the first
multichannel engine suitable for use in ZX Spectrum 16K programs.


Technical details

AMP16K is based on AMP, an older engine by Hikaru/Intense, and is largely identical to it. 
A special modification is used to make it work in contended memory. This is based on the 
following ideas and observations:

1. A looped or repeated code accessing lots of contended RAM, that is approaching the 
duration of one scanline (224 T states on a 16/48K Spectrum), would exhibit a tendency to
'snap' to this duration. This means that as long the code has certain duration that is less 
than 224 T states, the amount of slowdown applied to it by the ULA during screen time would 
be constant.

2. Because the inner loop code would be slowed down by a constant amount during screen time,
it is possible to slow down the routine during border time as well using additional delay 
code, so as to match that speed. Each frame, the inner loop is modified to run with this 
extra delay during border time, whereas during screen time it is changed back to run without
it. This is the key idea to make it work in contended memory.

As in the original AMP, the song data parser has branching and uneven timing. To accommodate
for this, the parser is run at the end of screen time, and mode 2 interrupts are used to
even out the timings.

In order to run equally in contended memory as well as uncontended, the engine performs a 
CPU speed test in the beginning. If the test result is above a certain threshold, the loop 
modifying code is made inactive, and the inner loop runs with the extra delay on at all 
times.

There's an attempt at balancing the timings of the loop modifying code and other things. 
This doesn't entirely work due to heavy contention and other reasons. However, the influence
on the resulting sound, if any, appears insignificant, so it was decided not to pursue it 
further. =)


Usage

Use the included XM module template.xm for composing your song. The overall sound of the
template module is intended to give a rough approximation of how the music would sound on
the actual ZX Spectrum.

It is recommended to use MilkyTracker for editing the XM modules. Warning: OpenMPT Tracker is
NOT recommended, as it is known to produce non-standard XM modules regardless of the export
settings!

Use instruments 1~8 to set the corresponding pulse width for notes in the tone channels (1~4).
A note must be present each time there is a change in the pulse width.

Drums (instrument 9, notes C-3 to G#3) can be put in any channel, but only one per row
is allowed.

The song tempo is controlled by the Speed setting alone, whereas the BPM setting is ignored.
The tempo is set on a per-pattern basis, and is reset to the Default Tempo value of an XM
module at the beginning of a pattern. You can use the Fxx command (xx < 32) in the first row
of a given pattern to change its tempo.
It is recommended to leave the BPM at its default value of 125 in the tracker. In that case,
the song playback speed in the tracker will largely correspond to how it would be played on
the Spectrum.

Once your song is ready, use ampconv.exe to convert it to an .ASM source file (SjASMPlus 
syntax). Include this converted .ASM source together with the main amp16k.asm source file in
your program, and CALL AMP16K with the address of your song in HL in order to play it. An 
example of this is provided in test.asm.

An example CMD script compile.bat will convert the demo song.xm file in the same directory,
and compile test.asm, producing a ZX Spectrum snapshot file which can be opened by emulators.


Notes

AMP16K uses interrupt mode 2 without the full 256-byte interrupt vector table. As such,
it is presumed incompatible with a number of peripherals that put random data on the bus
(DivIDE, Kempston interface...).

The engine sets up its ISR routine at the address #7E5C, restoring its previous contents
at exit. There is also an option to use the address #5C65 instead. This can be enabled by
including the following line in your source:
	DEFINE _AMP16K_LOW_ISR

The engine uses the full note range of the XM format (eight octaves).


AMP song data format

+0 (WORD) Offset from the pattern list to pattern data.
+2        Pattern list. Each entry is a WORD offset to the given pattern from the beginning
          of the pattern data.
+? (WORD) Pattern list loop. Stored as a negative offset from ($+2) to the loop position 
          in the list.
+?        Pattern data.


Pattern format

+0 (BYTE) Pattern speed, frames per row.
+1        Pattern row data.

Byte #01 is used to mark the end of pattern.


Pattern row format

+0 (BYTE) %1234 dddd, - Note map, Drum (if present). Note map is 4 bits saying if there's
          a note for the corresponding channel on this row.
+1 (BYTE) Channel note. Codes 1~97 represent notes C-0 to B-7. Code 0 is rest note.
          If bit 7 of a note is set, a pulse width BYTE for this channel follows.


The pulse width bytes represent sequences of bits that are ORed with the mixer register
whenever a channel accumulator overflows. They are defined using the formula "2^PW - 1",
where PW is the pulse width level (1~8).


(c) Intense 2020