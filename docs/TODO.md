# TODO

This list of items that still need to be done in Bintracker complements the [Issue Tracker](https://github.com/utz82/bintracker/issues).

Items marked with **[*]** are potentially good issues to work on as a new contributor, given some skill and knowledge in the relevant areas.

This list may be out of date. As a general rule of thumb, check back with us before starting to work on an issue.


## UI/UX

### Accessibility

- **[*] Write a high contrast color scheme**
- **User testing**
- **Explore possibilities for screen reader integration**
- **Implement zoom** with Ctrl-+/--/0.

### Design

**[*] Design custom button icons for various special tasks.** At the moment Bintracker uses [Material Design icons](https://material.io/resources/icons/), but there's a few tracker specific things that are missing from that set. Of course Material includes buttons for Copy/Cut/Paste and so on. However, in a tracker, there are for example two different ways to Cut. You can either cut a section and leave the following section in place, or you can cut a section and shift up whatever comes after it. Since we want a toolbar button for each of those, this of course requires two different button icons. Paste is even worse. Bintracker supports 4 different paste operations:

- Paste and shift down the following data (aka insert)
- Paste Over (replace the following data without shifting)
- Porous Paste Over (like Paste Over, but do not paste the empty steps, ie. if there is no value on a given row in the data that's pasted, then the old data is preserved)
- Porous Paste Under (the inverse version of Porous Paste Over - paste over the old data, but only paste on rows that are empty in the old data).

Other icon sets we've looked into so far (Feather, FontAwesome, IonIcons, Linea, OpenIconic) won't be of any help either, if anything they're even less complete. So there are basically two options: Either derive the missing icons from existing Material Design ones, or design a completely new, custom set of icons.



## Infrastructure

- **Set up linked permanent IRC/Discord channels.**
- **[*]** Set up and/or self-host synchronized **backup remote git repositories**.
- **Set up continuous integration.**
- **Set up a wiki.**



## Documentation

- **Create an index of symbols** exported by Bintracker/MDAL/Schemta that are visible at runtime



## Housekeeping

- **Collect licenses of used 3rd party components.**



## Build System

- **Call libmdal/Makefile recursively** from the main Makefile, to ensure that libmdal is rebuild as required. Currently libmdal must be rebuilt manually after changers.

- **[*] Transition to out-of-source-builds.**.

- **[*] Better detection of the user's build environment.**

- **[*] Build Bintracker on MacOS.** Building Bintracker itself shouldn't be too terribly hard. However we need to provide a convenient, easy-to-install package to users that will download and set up the necessary 3rd party software (MAME and Tcl/Tk) as well. As far as I know, Tcl/Tk is installed by default up to Mojave, but Apple stopped shipping it in Catalina. Also, ideally we'd want a CI pipeline for automatically building and deploying new releases.

- **[*] Build Bintracker on Windows.** See notes on building for MacOS above. Building for Windows will be more difficult though. We need a MSYS or MinGW based build system, and we need to arrange for a static build.



## Schemta

- **Rework Schemta to use local state** bound to a specific assembly process.
- **Add more instruction sets.** At this point we should prioritize 6502, 6809, F8, and 68000.


## Emulation

- **Write additional emulation backends**. VICE looks doable, at least.
- Look into **Hardware backends**, use actual target hardware as emulation backend. We could at least support some common microcontroller boards.
