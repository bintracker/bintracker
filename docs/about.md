# About Bintracker

Bintracker is a programmable chiptune music editor that supports many different chiptune drivers on a wide range of target platforms. Bintracker has strong [music tracker](https://en.wikipedia.org/wiki/Tracker_(music_software)) roots, but also incorporates a range of features normally found in [digital audio workstations](https://en.wikipedia.org/wiki/Digital_audio_workstation) and [audio programming languages](https://en.wikipedia.org/wiki/Audio_programming_language).

Making music on sound chips is a process strongly shaped by limitations. Bintracker's goal is to make sure artists can explore those limits to the fullest.


### Comprehensive Platform Support

Bintracker aims to be a Swiss Army Knife of Chipmusic, allowing musicians to utilize a wide range of platforms and music engines through a single tool. This means providing support for as many 8- and 16-bit computers and consoles as possible, and supporting various different sound drivers on these platforms. The range of supported chiptune drivers and target platforms can be easily extended. Eventually, Bintracker will extend support beyond the home computer era to include historical mainframe and minicomputers, as well as modern micro-controllers. A special place is reserved for "underdog" platforms, that is, uncommon platforms that are not supported by any other modern music editor.


### A Power Tool for Chipmusic Composers

While Bintracker should be easy enough to pick up even for chipmusic beginners, it is mainly geared towards more experienced composers who are looking for a powerful, advanced tool to aid their creative process. Bintracker's feature set is extensive, and can be extended further through the use of plugins. User configuration covers virtually any aspect of the application. Bintracker is a tool that you can adapt to your needs and habits, rather than you adapting to the tool.


### Tracker Evolution

Bintracker is not a tracker, it is a live programming environment running a tracker application by default. It is also a laboratory for experimenting with the core elements of trackers interfaces and trying out new ideas and concepts.


### Digital History Exploration

Bintracker is a conscious effort in computer archaeology. It enables researchers and artists alike to explore the evolution of computer music through experimental means. While the current focus is on the home computer era, Bintracker eventually aims to extend support right up to humble beginnings of computer music in the late 1940s.



## Philosophy

### Trackers As A Programming Language

Chip music tracker modules are commonly understood to be data. However, one can also interpret them as a script, a set of instructions that will be interpreted in a virtual machine (the music engine). In this sense, music trackers become domain specific programming environments.

Bintracker is centered around the idea of being an audio-visual programming tool. It expands on the concept of a music tracker by adding a meta-layer in form of a general purpose language interpreter. This language interpreter enables the user to create music in many different ways beyond the tracker metaphor, as well as modify and extend the behavior of Bintracker itself.



## Technology

In essence, Bintracker is an integrated development environment for the [Music Data Abstraction Language (MDAL)](mdal-introduction.md).

Bintracker is implemented in the [Scheme programming language](https://en.wikipedia.org/wiki/Scheme_(programming_language)), specifically [Chicken Scheme](https://call-cc.org/). As Bintracker incorporates a complete Scheme runtime, it can be reprogrammed from within itself. In other words, Bintracker is fully [hackable](hacking.md).

The Bintracker GUI uses the [Tk widget toolkit](https://en.wikipedia.org/wiki/Tk). The full Tk feature set is available from within the application at runtime, which means you can customize and extend Bintracker in any way you wish.

Bintracker does not handle emulation itself. Instead, it relies on [MAME™](https://www.mamedev.org) as an emulation backend. Support for other emulators can be added by providing an interface wrapper.



## Security Advice

Bintracker is an *unsafe* tool, due to incorporating a fully featured programming language interpreter. If you are concerned about system integrity, you should consider running Bintracker in a container or virtual machine.



## License

The Bintracker core and libmdal are released under the terms of the [MIT License](https://opensource.org/licenses/MIT). A number of third-party components that Bintracker incorporates or depends on are using similarly permissive licenses, including the [Apache License](https://opensource.org/licenses/Apache-2.0), the [BSD 2-clause License](https://opensource.org/licenses/BSD-2-Clause), and the [BSD 3-clause License](https://opensource.org/licenses/BSD-3-Clause).
