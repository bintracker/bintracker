# Configuration

Bintracker's behavior and appearance can be configured in a number of different ways. The most common way is to set various configuration options in the [configuration file](#configuration-file-configscm). You can also temporarily change these options by using Bintracker's built-in [repl](#temporary-tweaks-using-the-repl).

The configuration system in Bintracker is very powerful. In fact, not just the look and feel, but just about any aspect of how Bintracker works can be modified. To learn how to configure Bintracker in more fundamental ways, check out the [Advanced Configuration](#advanced-configuration) section.


## Configuration File: `config.scm`



### General Configuration Options

option             |default     | affects
-------------------|------------|-----------------------------
keymap             | `"EN"`     | default keymap
number-base        | `16`       | The numeral system used. Set to 10 for decimal, or 16 for hexadecimal numbers.
mdal-config-dir    |            | The directory containing the MDAL configuration files.
show-menu          | `#t`       | Enable or disable the main menu.
font-mono          | `'Courier` | The monospaced font used. Does not actually have to be a monospaced font, but your layout might look very strange if it isn't. Any of your system's fonts, or the built-in Tk font `Courier` may be used. When using system fonts, supply their name as a string enclosed in double quotes.


### Color Options

To change Bintracker's color scheme, use the `set-color!` command. For example, to change the default background color of the rows in the main editor to red, add the following line to your `config.scm`:

```scheme
(set-color! 'row "#ff0000")
```

Instead of RGB color codes, you can also use any of the [color names](https://www.tcl.tk/man/tcl/TkCmd/colors.htm) recognized by the Tk widget toolkit.

The following color options are available:

option              | affects
--------------------|-------------------------------------------------------
row                 | background color of regular rows in the editor
row-highlight-major | background color of major highlight rows in the editor
row-highlight-minor | background color of minor highlight rows in the editor
text                | text color of the editor
console-bg          | background color of the console
console-fg          | text color of the console

### Keybindings

## Temporary Tweaks Using The REPL

All the configuration options available through `config.scm` can also be set in the built-in REPL. However, changes made here are only temporary. That means those changes will not persist through an application restart.

Configuration options set throught the console may not take effect immediately. To force a refresh after your changes, you can try to run the
```scheme
(reconfigure)
```
command. Currently there is no guarantee that this will work.

You can check your current configuration settings using the `settings` and `colors` procedures.

## Advanced Configuration

The `config.scm` file is actually executed as a script, which means you can add arbitrary code to it and have it run when the program starts up.
