# Configuration

Bintracker's behavior and appearance can be configured in a number of different ways. The most common way is to set various configuration options in the [configuration file](#configuration-file-configscm). You can also temporarily change some of these options by using Bintracker's built-in [repl](#temporary-tweaks-using-the-repl). Furthermore, any user code may read and change configuration options, as well as register new ones.

The configuration system in Bintracker is very powerful. In fact, not just the look and feel, but just about any aspect of how Bintracker works can be modified. To learn how to configure Bintracker in more fundamental ways, check out the [Advanced Configuration](#advanced-configuration) section.


## Configuration File: `config.scm`

On startup, Bintracker loads and evaluates the file `config/config.scm`. Through this file, you can set the color theme, font, configure keybindings, activate plugins, and run user code to tweak Bintracker's behavior.

The default configuration file that comes with Bintracker documents further details. Just read it to learn more about available configuration options.


## Temporary Tweaks Using The REPL

All the configuration options available through `config.scm` can also be set in the built-in REPL. However, changes made here are only temporary. That means those changes will not persist through an application restart.

Configuration options set throught the console may not take effect immediately. To force a refresh after your changes, you can try to call the `reconfigure` procedure without any arguments. Currently there is no guarantee that this will work.

You can check your current configuration settings using the [`settings`](generated/bt-types.md) and [`colors`](generated/bt-types.md) procedures.
