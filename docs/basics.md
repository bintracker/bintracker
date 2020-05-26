# Bintracker Basics

This article gives a condensed overview of the things you need to know in order to get started with Bintracker.


### IMPORTANT: Security Advice

Despite the name, Bintracker is not a tracker. It is a virtual machine running a tracker application by default. However, the virtual machine can also run other code, which means it can do *bad things* to your computer. Things that can run code in Bintracker include plug-ins, configuration files, modules (.mmod), and engine definitions (.mdef) including support files.

**As a rule of thumb, never open files, install add-ons, or run code snippets from untrusted sources on the internet, unless you have checked them and understand what they do.**


### Differences from Other Trackers

At a basic level, Bintracker is quite similar to other chiptune trackers. If you have used any modern tracker like Famitracker, OpenMPT, or TIAtracker, you will probably feel at home easily. There are, however, a few notable differences.

- Bintracker is a meta-tracker, meaning it supports different sound engines on various platforms. How the Bintracker interface looks depends largely on the sound engine you use. Some engines may have a very unusual layout that is not very tracker-like.

- The concept of "pattern length", eg. the idea of a pattern consisting of a given number of rows, does not exist in Bintracker. Patterns (as well as any other block types such as effects tables) are endless by default. You use the sequence to control pattern lengths.

- [Sequences](#sequences) are more complex than in other trackers, however you can actually ignore them. See the section on sequences below for more details.

- Bintracker comes with fewer safety guarantees than other trackers. Normally, a tracker will ensure that the user can only input valid, sensible data, and will never produce output that breaks on the target machine. Bintracker validates input by type checking, but it does not care about sensibility. It does not prevent you from using tones outside of the audible range, setting an absurdly high BPM value, or embedding malfunctioning assembly code.

- Conceptually, there is no distinction between patterns, effects tables, samples, or other structures that hold recurring data. However, this has few practical consequences. Most importantly, this means that copying data between different structures is always possible in Bintracker, as long as the source and target use a compatible atomic datatype. For example, you can freely copy data from patterns to effects tables and vice versa. You can even do crazy things like copying data from a sample to a sequence.

- Bintracker auto-optimizes the song data, which means you do not need to worry about optimizing for size. In fact, it most likely will not have the intended effect: The MDAL module structure is virtual, which means there is no 1:1 correspondence between what you see in Bintracker, and what the target sound engine uses.


## Editing Modules

### Sequences

In Bintracker, sequences (also known as song orders) are quite complex, and somewhat different from other trackers. Fortunately, you can avoid dealing with sequences altogether by using [automatic sequencing](#automatic-sequencing).

Sequences are regular MDAL blocks, which means editing them works exactly like editing a pattern or an effects table.

Bintracker offers 3 different sequencing modes: Low-level, compact, and automatic. Compact sequences are an abstraction layer on top of low-level sequences, and automatic sequences are an abstraction layer on top of compact sequences. You can switch from a lower level mode to a higher one at any time. However, doing so will mutate the song. This means that while you switch from a lower level mode to a higher one and back, your low-level sequence will not be the same afterwards.

In terms of generated output data size, all three sequencing modes are equally efficient. So simply chose the sequence mode that best fits your work flow.

**TODO** Currently only low level mode is implemented.


#### Low Level Sequence Mode

Bintracker's low level sequences are multi-track, or "matrix" sequences, as used in Famitracker and many native chipmusic trackers. Each channel in the sound engine has its own track in the sequence. Pattern numbers are unique per track. So pattern 00 in track 1 is different from pattern 00 in track 2, for example.

The first track in this sequence mode (labelled "ROWS") is the step length. Patterns in Bintracker are "endless" by default, which means you need to specify in the sequence how many rows of pattern data are used to form a step in the sequence.

The remaining tracks correspond to the channels of the sound engine. Some engines may define additional sequence tracks in the sequence to handle transpose commands etc.


#### Compact Sequence Mode

Compact sequences are similar to single-track sequences that you may be familiar with from XM trackers. All channels in the sound engine share a single, common track in the sequence.

The first column in compact mode is identical to the step length (Rows) track in low level sequences. The second track is the combined pattern number for all channels. An engine may define additional tracks, see above.


#### Automatic Sequence Mode

Automatic sequencing removes the need to deal with sequences altogether. Instead of a sequence with multiple steps, you just get a single, endless pattern.
