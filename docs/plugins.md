# Plugins

To activate plugins in Bintracker, you need to register them in your [`config.scm`](configuration.md) with the command:

```scheme
(plugins 'register "plugin1 plugin2 ...")
```

Some plugins provide functionality that can be bound to keyboard shortcuts. Register bindings in your [`config.scm`](configuration.md) with the command

```Scheme
(bind-keys! 'plugins 'KEY-EVENT 'ACTION)
```

where KEY-EVENT is an Tk event specifier (eg. `'<Alt-Key-X>`) and ACTION is the name of a keybinding action listed in the plugin documentation. It is recommended to bind plugin actions to Alt-Key-*some-uppercase-letter*, as bindings involving Alt-Shift are not used in Bintracker's default bindings.

**Shortcuts must be registered before the plugin itself!**


You can also install 3rd party plugins by dropping them in the `plugins` directory. Note that installing 3rd party plugins is a security risk: Make sure you trust the source, and understand what the plugin does, before installing. We do not provide support for 3rd party plugins.

Looking to develop your own plugins? This [guide to writing plugins](writing-plugins.md) explains how to do it.


## Official Plugins

Bintracker currently ships with the following plugins:

- [mml](plugins/mml/README.md): Simple MML reader.
- [prng](plugins/prng/README.md): Various pseudo-random number generators.
- [unzufall](plugins/unzufall/README.md): Generate (pseudo-)random data for MDAL modules.
