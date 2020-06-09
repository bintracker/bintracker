SLEIZSA TRIO v0.1
=================
by utz 01'2017 * irrlichtproject.de * github.com/utz82


About
=====

Sleizsa Trio is a three-channel music routine for the Fairchild Channel F video
game console. It uses an algorithm originally developed by Zilogat0r for the ZX
Spectrum beeper. Full features are:

- 3 channels of tone, 16-bit frequency dividers
- duty for tones can be configured globally
- noise mode for channel 3
- 2 interrupting click drums
- 6-bit tempo resolution


Requirements
============

To use Sleizsa Trio, you will need to following:

- dasm macro assembler (http://dasm-dillon.sourceforge.net)
- an XM tracker (must support XM version 1.04 - try http://milkytracker.org)
- MAME (http://www.mame.org - not required, but will come in handy)


Setup
=====

Install Milkytracker, dasm, and MAME, if you haven't already done so.
(If you do not wish to add these utilities to your search path, you can
specify the paths in compile.bat/compile.sh instead. Windows users can also
copy the relevent dasm and MAME files to /SleizsaTrio).

Get the Channel F ROM files and put them in SleizsaTrio/roms/channelf. These
files are not distributed with Sleizsa Trio, but you can get them in various
places, such as the original Sleizsa package for example.

Note that you can use the now defunct MESS instead of MAME, however you will 
need to edit the compile scripts accordingly if you do. As far as I know there
are no drawbacks to this, as Channel F emulation code in MAME has not had any
major changes since the MESS days.


How to Use
==========

Step 1: Make some music using the included music.xm template.
Step 2: Execute compile.bat (Win) resp. compile.sh (*nix).
Step 3: Profit!

You can use the included music.xm template to compose your tunes. It gives
only a rough approximation of how the music will sound on the actual console,
though.

Tones must be entered into tracks 1-3, using instrument 01. Valid notes
are A-0 - B-5.

On channel 3, noise can be played instead of tones. For this, use instrument 2.
Note that the actual noise is very different from the XM template. It does 
change with pitch, but not in a linear manner. For best results, use notes A-5
or A#5.

There are two interrupting drum sounds (instrument 03 and 04), they can only
be used in channel 4. Only one drum sound can be active at a given time.

You can change the tempo with the Fxx command, or globally with the tempo
setting. You cannot change the BPM. So, only values F01..F1F are allowed.
Beware that after each pattern, the speed will be reset to the global value.

You can set a loop point with command Bxx. If no loop point is specified, the 
player will loop back to the start once it has finished the playing the song.

Other than Fxx/Bxx, all effects will be ignored. Changes made to the instruments
as well as volume settings are also ignored.

You can change the duty cycle (pulse width) settings globally for each channel 
by editing lines 36-38 of main.asm. Higher values may give more beefy sound, but 
have a higher chance of overloading/glitching the output. As a rule of thumb,
stick to values lower than $40.

If you are running out of space in the binary, try increasing the value in
line 34 of main.asm. In theory, up to 62 KB are possible, but MAME will break
when trying to use more than 8 KB.

I tried my best to correct the drum speed shift. If you do however find it to be
insufficient, you can try to experiment with the values in line 95 resp. 145
of main.asm.


Note to Programmers
===================

You are free to use Sleizsa Trio in your programs, as long as you credit me.
Beware that Sleizsa Trio is not guaranteed to work on all Channel F versions.

The routine normally resides at $800, but can be relocated to any other address.
The musicData section can reside anywhere in memory. Sleizsa Trio takes all the 
CPU time, so you can't do much else while music is playing. You could, however,
update graphics between notes (calling gfx code in the .readPtn section), at the
cost of introducing additional noise during note transitions.

Scratchpad registers r0-r16 including the K register are used, so you'll have
to work around that.

In case you are using external interrupts in your homebrew, you must disable
interrupts prior to calling Sleizsa Trio.

 However, it is possible to update graphics within the sound loop
(thereby lowering pitch and decreasing overall sound quality) as long as your
gfx code always takes a fixed number of cycles and restores scratchpad contents
on return (except Q which may be trashed).


Music Data Layout
=================

The first section is contains the order in which patterns will be played. It
consists of word-length pointers to the patterns and is terminated with an $ff
byte.

The following sections are the actual patterns, containing the music data.
Patterns can be of arbitrary length, and must be terminated with a $00 byte.

Each row in the pattern data consists of 4 bytes. The first byte contains
information about the speed of the row. A higher value corresponds to a lower
speed. The speed value must be >0 and <$40. Setting bit 6 of the first byte
will trigger the kick drum, setting bit 7 will trigger the noise drum. Only one
drum can be triggered on any given row. The noise drum takes precedence over the
kick drum.

Bytes 2-4 of a row are pointers to note values for channel 1-3, respectively. A 
value of 0 signifies silence, available notes are 2-$7e. Note values must be
even, except on channel 3, where uneven notes trigger the noise mode.


Thanks...
=========

... to B00daW for the original Sleizsa routine.
... to e5frog for veswiki.com
... to SeanRiddle, Blackbird, Urchlay, MAMEdev, and others for documenting 
    the Fairchild Channel F and the F8 chip.
