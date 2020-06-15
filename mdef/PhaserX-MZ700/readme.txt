********************************************************************************
PhaserX
by utz 09'2016 * www.irrlichtproject.de
********************************************************************************


ABOUT
=====

PhaserX is an experimental 2-channel beeper engine for the ZX Spectrum. It uses
a new type of effect called "duty modulation". As the name suggests, this effect
modulates the duty cycle setting over time. Its operating speed is synced to the
affected tone generator, so despite the similarities with the tone generation in
engines like Earth Shaker, it does not affect pitch.

Each of the two tone channels has a different set of effects. Channel 1 produces
a simple square wave. In addition to the aforementioned duty modulation, it
features a SID-like duty cycle sweep effect, and can generate pseudo-white noise
instead of square waves.

Channel 2 uses two oscillators to produce Phaser-style sound. Each of the
oscillators can have it's own duty modulation settings. This allows for very
complex timbres. Oscillator frequencies can of course be configured
independently, and can run at different phases. The channel mixer supports XOR
(Phaser1 standard), OR, and AND mixing methods. When using the OR method, the
oscillators can be decoupled and used as 2 independant channels, Squeeker style.

Credits go to Shiru for inventing the original Phaser effect.

Note that the engine does not check for Kempston interface presence.



COMPOSING
=========

There is currently no editor available for this engine, so in order to use it,
you'll have to hack up the music in Assembly. For details, check out the example
music.asm file, and the equates.h header.


MUSIC DATA
==========

The music data layout follows the usual sequence-pattern model.

The sequence is a list of pointers to patterns, in the order in which they are
to be played. A "loop" label must be present to specify which position the
player will loop to once the end of the sequence is reached. The sequence must
be terminated with a 0-word. Hence, the shortest possible sequence looks like
this:

loop
	dw pattern
	dw 0

Pattern data has a dynamic layout, using 2-9 words per row. Their function is
as follows:

1) speed (ticks_per_row * 256) + drum flags (1 = kick, 0x80 = hihat)
2) ch2_mixing_method * 256 + control flags
   mixing methods: 0xac = XOR, 0xb4 = OR, 0xa4 = AND
   ctrl flags: 1 = no update ch1, 0x40 = no update ch2
               4 = enable SID, 0x80 = enable noise
3) duty_modulator_ch1 * 256 + duty_ch1
4) note_divider_ch1
5) duty_modulator_ch2_oscA * 256 + duty_modulator_ch2_oscB
6) duty_ch2_oscA * 256 + duty_ch2_oscB
7) note_divider_ch2_oscA
8) note_divider_ch2_oscB
9) phase_offset_ch2_oscB

When the "no update ch1" flag is set, words 3 and 4 are omitted.
When the "no update ch2" flag is set, words 5-9 are omitted.

Initialization is mandatory for both channels at the beginning of the song.
Channel 2 must be updated at the beginning of each pattern.

Patterns are terminated with a 0x40 byte.
