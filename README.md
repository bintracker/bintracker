# Bintracker

**A hackable Chiptune Audio Workstation for the 21st Century**


![Bintracker Screenshot](docs/images/module-view-edit1.png?raw=true "Main Screen")


### [Website](https://bintracker.org) • [Documentation](https://bintracker.org/documentation)


## About

Bintracker is an advanced, cross-platform chiptune music editor that supports many different sound routines on a wide range of 8-bit and 16-bit target systems. It is also an interactive programming environment running on top of [Chicken Scheme](https://call-cc.org/).

At a basic level, Bintracker looks and feels like a [music tracker](https://en.wikipedia.org/wiki/Tracker_(music_software)), and can perfectly well be used as such. However, it also incorporates a range of features normally found in [digital audio workstations](https://en.wikipedia.org/wiki/Digital_audio_workstation) and [audio programming languages](https://en.wikipedia.org/wiki/Audio_programming_language).

Both the editor itself, as well as the range of supported chiptune drivers and target platforms can be extended through plugins.


## Current State

Bintracker is at alpha stage. While the basics are implemented, most of the advanced features are still in the making. Also, there are many bugs and missing bits and pieces here and there, so expect frequent crashes.

Things you can do at this point include:

- Compose music for various sound engines on a limited range of target systems
- Export music modules for target hardware
- Assemble code and control external emulators
- Control the application through the Scheme language shell (REPL)
- Add your own sound engines and plug-ins


### Target Systems

Currently, the following target systems are supported with one or more sound engines:

- Atari 2600/VCS
- Dragon 32
- Fairchild Channel F
- Exidy Sorcerer 4 Voice Music System
- Sinclair ZX Spectrum (beeper)
- Sharp MZ-700

Eventually, Bintracker will feature sound engines on a wide range of target platforms, with a focus on lesser known systems.


## Setup

Windows users can grab the latest alpha build on the [Release](https://github.com/bintracker/bintracker/releases) page. A bit of manual configuration is required, see the [instructions here.](https://github.com/bintracker/bintracker/blob/master/docs/setup.md#windows)

Users on other operating systems need to [compile Bintracker from source](https://github.com/bintracker/bintracker/blob/master/docs/setup.md#compiling-from-source). Only Linux builds have been tested so far. macOS builds should be possible with some minor adjustments, though this is currently unsupported. Please get in touch if you manage to get a macOS build running.


## Contributing

Contributions are most welcome! Besides working on the Bintracker source itself, there are [a lot of different tasks](https://bintracker.org/documentation/TODO.html) that need help, from designing icons, testing, writing demo tunes, creating tutorials and documentation, to running infrastructure, writing plugins, and adding support for new sound drivers.

Check out the [Contributor's Guide](https://bintracker.org/documentation/contributing.html) for more information on how to get started contributing to Bintracker. If you need help or have any questions, feel free to [open an issue](https://github.com/bintracker/bintracker/issues) here on Github, or [send us a message](https://bintracker.org/contact/).
