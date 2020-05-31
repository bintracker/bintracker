# Plugins

To activate plugins in Bintracker, you need to register them in your [`config.scm`](configuration.md) with the command:

```scheme
(plugins 'register "plugin1 plugin2 ...")
```

You can also install 3rd party plugins by dropping them in the `plugins` directory. Note that installing 3rd party plugins is a security risk: Make sure you trust the source, and understand what the plugin does, before installing. We do not provide support for 3rd party plugins.

Looking to develop your own plugins? This [guide to writing plugins](writing-plugins.md) explains how to do it.


## Official Plugins

Bintracker ships with the following plugins:

- [mml](plugins/mml/README.md): Simple MML reader
