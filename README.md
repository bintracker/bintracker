# Bintracker

**A hackable Chiptune Audio Workstation for the 21st Century**

### [Website](https://bintracker.org) • [Documentation](https://bintracker.org/documentation)


## About

Bintracker is an advanced, cross-platform chiptune music editor that supports many different sound routines on a wide range of 8-bit and 16-bit platforms. It is also a live programming environment running on top of [Chicken Scheme](https://call-cc.org/).

At a basic level, Bintracker looks and feels like a [music tracker](https://en.wikipedia.org/wiki/Tracker_(music_software)), and can perfectly well be used as such. However, it also incorporates a range of features normally found in [digital audio workstations](https://en.wikipedia.org/wiki/Digital_audio_workstation) and [audio programming languages](https://en.wikipedia.org/wiki/Audio_programming_language).

Both the editor itself, as well as the range of supported chiptune drivers and target platforms can be easily extended through plugins.


## Current State

Bintracker is currently at a very early alpha stage. While the basics are implemented at this point, most of the advanced features are still in the making. Also, there are many bugs and missing bits and pieces here and there, so expect

Things you can do at this point include:

- Compose music for 2 different ZX Spectrum 1-bit sound drivers
- Control the application through the interactive REPL shell
- Assemble code and control external emulators


## Setup

Currently the only way to run Bintracker is to [compile it from source](https://bintracker.org/documentation/setup.html). Only Linux builds have been tested so far. We plan to target Windows and MacOS as well, but at this point you're on your own when building on these platforms. We do appreciate if you try, though!


## Contributing

Contributions are very welcome! Besides working on the Bintracker source itself, there are [a lot of different tasks](https://bintracker.org/documentation/TODO.html) that need help, from designing icons, testing, writing demo tunes, creating tutorials and documentation, to running infrastructure, writing plugins, and adding support for new sound drivers.

Check out the [Contributor's Guide](https://bintracker.org/documentation/contributing.html) for more information on how to get started contributing to Bintracker. If you need help or have any questions, feel free to [open an issue](https://github.com/utz82/bintracker/issues) here on Github, or [send us a message](https://bintracker.org/contact/).
