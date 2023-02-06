# Additional Bindings

Bindings from the following Chicken Scheme modules and eggs (libraries) are available in Bintracker's top-level namespace in addition to the standard [R5RS Scheme](https://conservatory.scheme.org/schemers/Documents/Standards/R5RS/) bindings, the Bintracker API, libmdal, and Schemta:


### Chicken Modules

- [chicken.base](https://wiki.call-cc.org/man/5/Module%20(chicken%20base)) - core utility procedures and macros
- [chicken.bitwise](https://wiki.call-cc.org/man/5/Module%20(chicken%20bitwise)) - bitwise integer manipulation
- [chicken.condition](https://wiki.call-cc.org/man/5/Module%20(chicken%20condition)) - exception handling
- [chicken.file](https://wiki.call-cc.org/man/5/Module%20(chicken%20file)) - file system manipulation
- [chicken.io](https://wiki.call-cc.org/man/5/Module%20(chicken%20io)) - reading and writing files and ports
- [chicken.module](https://wiki.call-cc.org/man/5/Modules) - defining and using modules
- [chicken.platform](https://wiki.call-cc.org/man/5/Module%20(chicken%20platform)) - host system introspection
- [chicken.port](https://wiki.call-cc.org/man/5/Module%20(chicken%20port)) - manipulating port objects
- [chicken.random](https://wiki.call-cc.org/man/5/Module%20(chicken%20random)) - pseudo-random number generation
- [chicken.string](https://wiki.call-cc.org/man/5/Module%20(chicken%20string)) - string operations


### SRFIs

- [srfi-1](https://wiki.call-cc.org/eggref/5/srfi-1) - list library
- [srfi-13](https://wiki.call-cc.org/eggref/5/srfi-13) - string library
- [srfi-14](https://wiki.call-cc.org/eggref/5/srfi-14) - character set library
- [srfi-18](https://wiki.call-cc.org/eggref/5/srfi-18) - multi-threading
- [srfi-69](https://wiki.call-cc.org/eggref/5/srfi-69) - hash tables


### Eggs

- [bitstring](https://wiki.call-cc.org/eggref/5/bitstring) - binary data manipulation and pattern matching
- [comparse](https://wiki.call-cc.org/eggref/5/comparse) - PEG parser combinators
- [coops](https://wiki.call-cc.org/eggref/5/coops) - object system
- [list-utils](https://wiki.call-cc.org/eggref/5/list-utils) - additional convenience utilities for list manipulation
- [pstk](https://wiki.call-cc.org/eggref/5/pstk) - interaction with Tcl/Tk.
- [sqlite3](https://wiki.call-cc.org/eggref/5/sqlite3) (`execute` procedure only) - run statements on SQLite databases


For code running in the repl, the above bindings are automatically available. For plugin code, required modules must be explicitly [imported](https://api.call-cc.org/5/doc/chicken/modules/import).


## Requests

If you need any other bindings, and can make a compelling argument for why Bintracker should make them available, feel free to open a request on the [issue tracker](https://github.com/bintracker/bintracker/issues).

The following will **not** be considered for inclusion in Bintracker:

- libraries with limited platform support (eg. Linux-only libraries)
- anything not licensed under MIT, BSD, Apache, LGPL, PD/Unlicense/WTFPL, or similar terms
- libraries not available through Chicken Scheme's [extension registry](https://eggs.call-cc.org/5/)
