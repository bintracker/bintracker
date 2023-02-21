# The REPL

The REPL, or [Read-Eval-Print-Loop](https://en.wikipedia.org/wiki/REPL), is an interactive shell that you can use to interact with the live programming environment that powers Bintracker. The REPL provides an interpreter for the [Scheme programming language](https://en.wikipedia.org/wiki/Scheme_(programming_language)), specifically [Chicken Scheme](https://call-cc.org/). You can control the entire Bintracker application through the REPL.

To get started, type `(info)` and hit Enter (Return) at the `repl>` prompt at the bottom of the Bintracker main window. For more information, consult the [Developer Documentation](hacking.md).

!!! info ""

    Note that the REPL always uses decimal numbers, even when the `number-base` setting is not set to 10.


## Structured Editing

By default, [structured editing](https://en.wikipedia.org/wiki/Structured_editing) is enabled in the REPL. In this mode

- any opening parenthesis, bracket, brace, or double quote is automatically matched with its closing counterpart (except when escaped with a backslash or inside strings)
- closing parenthesis, brackets, and braces can not be entered, except when escaped or inside strings
- lists `(...)` and strings `"..."` can not be deleted with Backspace/Delete unless they are empty
- deleting the opening character of an empty list or string will also delete its closing counterpart
- pressing Enter (Return) with the cursor inside an expression inserts a line break
- pressing Enter with the cursor positioned after any expressions will evaluate the first expression at prompt.

When structured editing is disabled, Enter instead triggers evaluation whenever the text entered at the prompt forms a complete Scheme expression.

To disable structured editing, add the following line to your `config/config.scm`:

```scheme
(settings 'repl-enable-struct-editing #f)
```

You can also toggle this setting at runtime. To do so, run the above code in the REPL, followed by

```scheme
(repl-modify-default-events (repl))
```
