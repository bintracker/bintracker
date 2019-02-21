# Music Data Abstraction Language

The **Music Data Abstraction Language** (MDAL) is a veritable, text-based
[audio programming language](https://en.wikipedia.org/wiki/Audio_programming_language)
specifically tailored for use with sound drivers operating on low-level devices
such as [sound chips](https://en.wikipedia.org/wiki/Sound_chip) and simple
[DACs](https://en.wikipedia.org/wiki/Digital-to-analog_converter).

While traditional low-level music description languages such as ABC or the Music
Macro Language are essentially abstractions of western sheet music notation,
MDAL instead has strong roots in [tracker music](https://en.wikipedia.org/wiki/Music_tracker).
The MDAL syntax adheres to several key elements of tracker modules:

* Song structure is broken down into patterns, which are linked via a sequence
  matrix.
* Time flow is represented vertically.
* The concept of measures and note values has no direct representation in MDAL,
  instead note length is measured in steps and ticks.

MDAL is a language without a pre-defined nomenclature. Aside from a rudimentary
set of structural conventions, there are no fixtures - the entire instruction
set is customizable. Customization is provided through standardized
configuration files, which are parsed and interpreted by an MDAL compiler at
runtime. This enables programmers to adapt both user input and data output to
the specific needs of their sound drivers/player routines.
