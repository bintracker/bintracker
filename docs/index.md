# Bintracker NG

**a hackable chiptune audio workstation for the 21st century**


## About Bintracker NG

Bintracker NG is an open source, cross-platform chiptune music editor that supports many different chiptune drivers on a wide range of target platforms.

At a basic level, Bintracker NG looks and feels like a [music tracker](https://en.wikipedia.org/wiki/Tracker_(music_software)), and can perfectly well be used as such. However, it also incorporates a range of features normally found in [digitial audio workstations](https://en.wikipedia.org/wiki/Digital_audio_workstation) and [audio programming languages](https://en.wikipedia.org/wiki/Audio_programming_language).

Both the editor itself, as well as the range of supported chiptune drivers and target platforms can be easily extended.


### Technology

In essence, Bintracker NG is an integrated development environment for the [Music Data Abstraction Language (MDAL)](../libmdal/docs/index.md).

Bintracker NG is implemented in the [Scheme programming language](https://en.wikipedia.org/wiki/Scheme_(programming_language)), specifically [Chicken Scheme](https://call-cc.org/). As Bintracker NG incorporates a complete Scheme runtime, it can be reprogrammed from within itself. In other words, Bintracker NG is fully [hackable](hacking.md).

The Bintracker NG GUI uses the [Tk widget toolkit](https://en.wikipedia.org/wiki/Tk). The full Tk feature set is available from within the application at runtime, which means you can customize and extend Bintracker NG in any way you wish.

Bintracker NG does not handle emulation itself. Instead, it relies on [MAME™](https://www.mamedev.org) as an emulation backend. Support for other emulators can be added by providing an interface wrapper.


## Philosophy

### Trackers as a Visual Programming Language

### User Freedom

Bintracker NG prioritizes user freedom whenever possible. ... However, as the old saying goes: With great power comes great responsibility. Bintracker NG does not prevent you from shooting yourself in the foot. It is entirely possible to create tracks that will not work properly or even crash the target platform. So far, nobody has blown up their hardware with Bintracker tracks, but we don't completely rule out the odds of this happening either ;)


## License

The Bintracker NG core and libmdal are released under the terms of the [MIT License](https://opensource.org/licenses/MIT). A number of third-party components that Bintracker NG incorporates or depends on are using similarly permissive licenses, including the [Apache License](https://opensource.org/licenses/Apache-2.0), the [BSD 2-clause License](https://opensource.org/licenses/BSD-2-Clause), and the [BSD 3-clause License](https://opensource.org/licenses/BSD-3-Clause).
