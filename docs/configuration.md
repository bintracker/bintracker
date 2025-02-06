# Configuration

Bintracker's behavior and appearance can be configured in a different ways. The most common way is to set various configuration options in the [configuration file](#main-configuration-file-configscm). You can also temporarily change some of these options by using Bintracker's built-in [repl](#temporary-tweaks-using-the-repl). Furthermore, any user code may read and change configuration options, as well as register new ones.

Bintracker has a powerful configuration system. Not just the look and feel, but just about any aspect of how Bintracker works can be modified.


## Main Configuration File: `config.scm`

On startup, Bintracker loads and evaluates the file `config/config.scm`. Through this file, you can set the color theme, font, configure keybindings, activate plugins, and run user code to tweak Bintracker's behavior.

The default configuration file that comes with Bintracker documents further details. Just read it to learn more about available configuration options.


### Key Bindings

You can supply your custom keymap by copying one of the maps provided in `config/keymaps` and changing bindings according to your needs. The key specifier must use Tk's [angular bracket syntax](https://www.tcl.tk/man/tcl8.6/TkCmd/bind.htm). Make sure your keymap has a unique name and uses the `.keymap` extension. To enable your keymap, use `(load-keymap "MAP-NAME"")` in the main configuration file.

You can also use the main configuration file to override selected key bindings with the [`bind-keys!`](generated/bt-state.md#def-bind-keys!) procedure.

!!! danger "WARNING"

    Do not rebind keyboard shortcuts that would normally trigger a menu action, as this may cause the user interface to become unstable.


### Color Schemes

You can tweak Bintracker's color schemes, or provide your own. See `config/color-schemes` for some pre-defined examples. To load a color scheme, run `(colors 'load "SCHEME-NAME")` in the main configuration file.


## Emulation Configuration

Bintracker relies on external tools for emulation. You can configure these tools by editing the file `config/emulators.scm` on *nix, and `config/emulators.windows.scm` on Windows. The respective files contain a list in the format

```scheme
((EMULATOR-ID program-name: PATH-TO-EXECUTABLE default-args: LIST-OF-ARGS)
 ...)
```

EMULATOR-ID is an arbitrary, unique identifier for the list entry. PATH-TO-EXECUTABLE is either the name of an installed executable, a full path to an executable, or a path relative to Bintracker's base directory. LIST-OF-ARGS is a list of command line arguments that will be passed to the emulator.

Currently, Bintracker supports only [MAME](https://www.mamedev.org/) as emulation back-end. On *nix, Bintracker expects MAME to be installed on the system as `mame64`. On Windows, Bintracker expects `mame.exe` in `3rdparty\mame`. You can change this behavior (for example to use a system installed MAME on Windows) by editing the `program-name` parameter. Bintracker expects ROM files to be located in `roms`. You can change this by supplying a different value to the `-rompath` argument.

### Target System Configuration

You can add support for additional target systems by adding an MDAL target specification in `mdal-targets`, and an entry to `config/systems.scm`. The target specification takes the format

```scheme
(id: "SYSTEM-ID"
     cpu: CPU-ID
     clock-speed: N
     default-start-address: S
     exports: (ADDITIONAL-FORMATS...))
```

SYSTEM-ID is a string uniquely identifying the target system. CPU-ID names the system's main CPU (see `mdal-targets/cpu` for a list of supported CPUs.) N is the system's clock speed in Hertz. S is the default entry point for machine code. ADDITIONAL-FORMATS... may specify additional export formats (currently only supports ZX Spectrum .tap).

`systems.scm` contains a list in the format

```scheme
("SYSTEM-ID" emulator: EMULATOR-ID startup-args: (...)
 ...)
```

SYSTEM-ID is a string naming an MDAL target specified in `mdal-targets`. EMULATOR-ID is the identifier for the emulator to use, as specified in `emulators[.windows].scm`. STARTUP-ARGS is a list of additional arguments to be passed to the emulator on the command line. For MAME, this list must at least contain the MAME name of the target system (see the [Arcade Database](http://adb.arcadeitalia.net/) for a list of valid names).


## Temporary Tweaks Using The REPL

All the configuration options available through `config.scm` can also be set in the built-in REPL. However, changes made here are only temporary. That means those changes will not persist through an application restart.

Configuration options set throught the console may not take effect immediately. To force a refresh after your changes, you can try to call the `reconfigure` procedure without any arguments. Currently there is no guarantee that this will work.

You can check your current configuration settings using the [`settings`](generated/bt-types.md) and [`colors`](generated/bt-types.md) procedures.
