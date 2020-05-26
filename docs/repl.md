# The REPL

The REPL, or [Read-Eval-Print-Loop](https://en.wikipedia.org/wiki/REPL), is an interactive shell that you can use to interact with the live programming environment that powers Bintracker. The REPL runs on an interpreter for the [Scheme programming language](https://en.wikipedia.org/wiki/Scheme_(programming_language)), specifically [Chicken Scheme](https://call-cc.org/). You can control the entire Bintracker application through the REPL.

To get started, type

```Scheme
(info 'repl)
```

at the `repl>` prompt at the bottom of the Bintracker main window.

Note that the REPL always uses decimal numbers, even when the `number-base` setting not set to 10.

For more information on how to interact with Bintracker through the REPL, consult the [Developer Documentation](hacking.md).
