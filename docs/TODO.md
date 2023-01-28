# TODO

This list of items that still need to be done in Bintracker complements the [Issue Tracker](https://github.com/bintracker/bintracker/issues).

Items marked with **[*]** are potentially good issues to work on as a new contributor, given some skill and knowledge in the relevant areas.

This list may be out of date. As a general rule of thumb, check back with us before starting to work on an issue, using the [issue Tracker](https://github.com/bintracker/bintracker/issues) or the [contact form](https://bintracker.org/contact/).


## UI/UX

### Interface

- **Option to disable scrollbars**
- **i18n**: Separate print strings and provide translations.
- **[*] More keymaps**

### Discovery

- **Display favorite MDEFs and recently used MMODs in welcome buffer**
- **Internal documentation system**. The plan is to make the `info` command the central entry point to the internal help/documentation system. Internal documentation will draw from various sources (Bintracker source, keybinding info, MDEFs, plugins...). The internal doc system should be extendable, so plugins could collect further documentation from external sources (CPU instruction sets, hardware specifications, etc.)

### Design

**[*] Design custom button icons for various special tasks.** At the moment Bintracker uses [Material Design icons](https://material.io/resources/icons/), but there's a few tracker specific things that are missing from that set. Of course Material includes buttons for Copy/Cut/Paste and so on. However, in a tracker, there are for example two different ways to Cut. You can either cut a section and leave the following section in place, or you can cut a section and shift up whatever comes after it. Since we want a toolbar button for each of those, this of course requires two different button icons. Paste is even worse. Bintracker supports 4 different paste operations:

- Paste and shift down the following data (aka insert)
- Paste Over (replace the following data without shifting)
- Porous Paste Over (like Paste Over, but do not paste the empty steps, ie. if there is no value on a given row in the data that's pasted, then the old data is preserved)
- Porous Paste Under (the inverse version of Porous Paste Over - paste over the old data, but only paste on rows that are empty in the old data).

Other icon sets we've looked into so far (Feather, FontAwesome, IonIcons, Linea, OpenIconic) won't be of any help either, if anything they're even more lacking. So there are basically two options: Either derive the missing icons from existing Material Design ones, or design a completely new, custom set of icons.


### Accessibility

- **[*] Write a high contrast color scheme**
- **[*] User testing**
- **Improve screen reader/text-to-speech integration**
- **Implement zoom** with Ctrl-+/--/0.



## Infrastructure

- **Set up linked permanent IRC/Discord channels.**
- **[*]** Set up and/or self-host synchronized **backup remote git repositories**.
- **Set up continuous integration.** This is a difficult task, but I would be very happy if I don't have to do it myself.
- **Set up a bugtracker.** To minimize the risk of getting vendor-locked by Github, we should use an external ticket system. Ideally it would sync issues from Github, though.
- **Set up a wiki.**



## Documentation

- **Create an index of symbols** exported by Bintracker/MDAL/Schemta that are visible at runtime
- **Create a list of common high-level entry points**. This requires definition anchors to be implemented in scm2wiki, so the list can link directly to the generated documentation.
- **Register scm2wiki as an official Chicken Scheme extension**. This eliminates the need to download and install scm2wiki separately.

## Build System

- **[*] Better detection of the user's build environment.**

- **[*] Build Bintracker on MacOS.** Building Bintracker itself shouldn't be too terribly hard. However we need to provide a convenient, easy-to-install package to users that will download and set up the necessary 3rd party software (MAME and Tcl/Tk) as well. As far as I know, Tcl/Tk is installed by default up to Mojave, but Apple stopped shipping it in Catalina. Also, ideally we'd want a CI pipeline for automatically building and deploying new releases.

- **[*] Build Bintracker on Windows.** See notes on building for MacOS above. Building for Windows will be more difficult though. We need a MSYS or MinGW based build system, and we need to arrange for a static build.

- **Collect licenses of used 3rd party components.** We'll need to ship these with executable builds.


## Schemta

- **Add more instruction sets.** At this point we should prioritize Motorola 68000.


## Emulation

- **Write additional emulation backends**. VICE looks doable, at least.
- Look into **Hardware backends**, use actual target hardware as emulation backend. We could at least support some common microcontroller boards.


## Plugins

- **Set up a separate, "official" plugin repository.**
- **Updates**


### Plugin Ideas

- **git** as low-level dependency for the Project Manager and Online Collab plugins.
- The **Project Manager** will allow users to put works under version control.
- **Online Collaboration**. This is still in the brainstorming phase, but I think something like remote shared buffers (as in, remote shared desktop) + git could work, eg. one user holds the "master" branch and shares their module buffer. I'll need someone who is competent with web security to help with this.
- **Snippets**, a database driven library of user-created patches, instruments, melodies, etc.
- **Fuzzy finder**. A tool that searches for features within modules (samples, sections, snippets).
- **Piano Roll**
- **Graph View**, a buffer that allows editing blocks as a set of drawable curves.
- **AlgoComp**, a collection of utilities for algorithmic music generation.
