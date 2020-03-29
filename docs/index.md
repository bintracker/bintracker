# Bintracker

**a hackable chiptune audio workstation for the 21st century**


## About Bintracker

Bintracker is an open source, cross-platform chiptune music editor that supports many different chiptune drivers on a wide range of target platforms.

At a basic level, Bintracker looks and feels like a [music tracker](https://en.wikipedia.org/wiki/Tracker_(music_software)), and can perfectly well be used as such. However, it also incorporates a range of features normally found in [digitial audio workstations](https://en.wikipedia.org/wiki/Digital_audio_workstation) and [audio programming languages](https://en.wikipedia.org/wiki/Audio_programming_language).

Both the editor itself, as well as the range of supported chiptune drivers and target platforms can be easily extended.


### Technology

In essence, Bintracker is an integrated development environment for the [Music Data Abstraction Language (MDAL)](../libmdal/docs/index.md).

Bintracker is implemented in the [Scheme programming language](https://en.wikipedia.org/wiki/Scheme_(programming_language)), specifically [Chicken Scheme](https://call-cc.org/). As Bintracker incorporates a complete Scheme runtime, it can be reprogrammed from within itself. In other words, Bintracker is fully [hackable](hacking.md).

The Bintracker GUI uses the [Tk widget toolkit](https://en.wikipedia.org/wiki/Tk). The full Tk feature set is available from within the application at runtime, which means you can customize and extend Bintracker in any way you wish.

Bintracker does not handle emulation itself. Instead, it relies on [MAME™](https://www.mamedev.org) as an emulation backend. Support for other emulators can be added by providing an interface wrapper.


## Philosophy

### Trackers as a Visual Programming Language

### User Empowerment

Bintracker aims to give as much power to the user as possible. Bintracker should be a tool that you can adapt to your needs, rather than you adapting to the tool. However, with great power comes great responsibility. Bintracker is by no means a *safe* tool. It does not prevent you from shooting yourself in the foot. Bintracker will happily wreck your file system if you ask it to, and more.


## License

The Bintracker core and libmdal are released under the terms of the [MIT License](https://opensource.org/licenses/MIT). A number of third-party components that Bintracker incorporates or depends on are using similarly permissive licenses, including the [Apache License](https://opensource.org/licenses/Apache-2.0), the [BSD 2-clause License](https://opensource.org/licenses/BSD-2-Clause), and the [BSD 3-clause License](https://opensource.org/licenses/BSD-3-Clause).
