# Frequently Asked Questions

## General

#### Can I do *X* in Bintracker?

The answer is probably yes, though it might not be convenient do to so (yet).

#### Any plans for Android/iOS support?

Not yet. We may support mobile devices some day but not in the near future.

#### I'm using a French/German/other non-standard keyboard. How can I change the key bindings?

Go to your Bintracker folder, and open the file `config.scm` from the `config` subfolder in a text editor. Find the line that says `(load-keymap "en")` and change the `"en"` part to `"fr"`, `"de"`, or whatever layout you use. See the `keymap` subfolder of the `config` folder for a list of available keymaps. If no suitable keymap exists, you can derive your own by copying one of the existing keymaps and editing it.

## Development

#### What do those `' `ticks and ``` ``backticks do?

It's syntactic sugar for `(quote ...)` and `(quasiquote)`. To oversimplify a lot, [quotation](https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Quoting.html) is Scheme's means of figuring out whether something is code or data. Normally expressions are considered to be code, and are evaluated as such when the interpreter encounters them. Quoting turns code into data.

Say you have defined the symbol `my-number` to be `1`. Typing `my-number` at the [REPL](repl.md) will return `1`, because that's what the symbol `my-number` evaluates to. Typing `'my-number` on the other hand gives you back the symbol `my-number`, because that's what the expression `(quote my-number)` evaluates to.

The backtick ``` `` does exactly the same as the tick `'`, but allows *unquoting* within the (quasi)quoted expression. A comma `,` indicates unquoting within an expression following a backtick. So `` `(my-number ,my-number)`` evaluates to the list `(my-number 1)`.

There's also unquote-splicing, but you don't need to worry about that until much later.
