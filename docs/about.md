# About Bintracker

Bintracker is an open source, cross-platform chiptune music editor that supports many different chiptune drivers on a wide range of target platforms.

At a basic level, Bintracker looks and feels like a [music tracker](https://en.wikipedia.org/wiki/Tracker_(music_software)), and can perfectly well be used as such. However, it also incorporates a range of features normally found in [digital audio workstations](https://en.wikipedia.org/wiki/Digital_audio_workstation) and [audio programming languages](https://en.wikipedia.org/wiki/Audio_programming_language).

Both the editor itself, as well as the range of supported chiptune drivers and target platforms can be easily extended.


### Technology

In essence, Bintracker is an integrated development environment for the [Music Data Abstraction Language (MDAL)](mdal-introduction.md).

Bintracker is implemented in the [Scheme programming language](https://en.wikipedia.org/wiki/Scheme_(programming_language)), specifically [Chicken Scheme](https://call-cc.org/). As Bintracker incorporates a complete Scheme runtime, it can be reprogrammed from within itself. In other words, Bintracker is fully [hackable](hacking.md).

The Bintracker GUI uses the [Tk widget toolkit](https://en.wikipedia.org/wiki/Tk). The full Tk feature set is available from within the application at runtime, which means you can customize and extend Bintracker in any way you wish.

Bintracker does not handle emulation itself. Instead, it relies on [MAME™](https://www.mamedev.org) as an emulation backend. Support for other emulators can be added by providing an interface wrapper.


## Philosophy

### Trackers as Programming Tools

Chip music tracker modules are commonly understood to be data. However, you could also interpret them as a script, a set of instructions that will be interpreted in a virtual machine (the music engine). In this sense, music trackers become domain specific programming environments.

Bintracker is centered around the idea of being an audio-visual programming tool. It expands on the concept of a music tracker by adding a meta-layer in form of a general purpose language interpreter. This language interpreter enables the user to create music in many different ways beyond the tracker metaphor, as well as modify and extend the behavior of Bintracker itself.

### Digital History Preservation

Bintracker is a conscious effort in digital history preservation. It enables researchers and artists alike to explore the evolution of computer music, with a current focus on the home computer era.

### Flexibility

Bintracker should be a tool that you can adapt to your needs and habits, rather than you adapting to the tool.

### User Empowerment

Bintracker aims to give as much power to the user as possible. ...

However, with great power comes great responsibility. Bintracker is by no means a *safe* tool. If you are concerned about system integrity, you should consider running Bintracker in a container or virtual machine.


## License

The Bintracker core and libmdal are released under the terms of the [MIT License](https://opensource.org/licenses/MIT). A number of third-party components that Bintracker incorporates or depends on are using similarly permissive licenses, including the [Apache License](https://opensource.org/licenses/Apache-2.0), the [BSD 2-clause License](https://opensource.org/licenses/BSD-2-Clause), and the [BSD 3-clause License](https://opensource.org/licenses/BSD-3-Clause).
