# Getting Started with Bintracker Development


## Prerequisites

As Bintracker is written in the [Scheme programming language](https://en.wikipedia.org/wiki/Scheme_(programming_language)), you will need some understanding of Scheme or another Lisp-like language. Although Scheme's syntax is very different from other programming languages, it is very easy to learn. Even a basic understanding is enough to start running some simple code in Bintracker's [REPL](repl.md) or tweaking [configs/config.scm](configuration.md).

If you plan on writing more than a few lines of code for Bintracker, you should consider using an editor with a structured editing mode. Vim, Atom, and VS Code all support something akin to Emacs' [paredit](http://danmidwood.com/content/2014/11/21/animated-paredit.html).




## Reading the Source

If this is your first time working iwht Scheme or another language from the Lisp family, reading the Bintracker source code can seem a bit intimidating. This section gives some general hints on how to find your way around in the Bintracker source.

1. There are no header files, as you might be used to when coming from C or C++. However, the compiler generates various header-like files during compilation. Any `.scm` file in the source tree whose name contains more than one dot are auto-generated, you usually can ignore those.

2. There is no explicit `main`. The top level source file is `bintracker.scm`, which recursively depends on all other files in the source tree.

3. Bintracker is split into several [core components](#core-components).

4. Read source files from the bottom. When writing Scheme code, the most high-level abstractions often end up near the bottom of the file.

5. Namespace management: In Chicken Scheme, [Modules](http://wiki.call-cc.org/man/5/Modules) are the main means of managing namespaces. Each source file in the main source directory and the libmdal sub-directory contains a single module.

6. There is a hierarchy of modules: `libmdal/mdal.scm` is the meta-module of all libmdal modules, and `bintracker-core.scm` is the top-level meta-module of Bintracker, which re-exports all bindings that will be visible to user code at runtime. To find out what a given module depends on, look for `(import ...)` statements near the top of an `.scm` file. If you don't find a module mentioned in an `import` statement in the source tree, then it is probably a [3rd party extension](#dependencies).

7. After startup, Bintracker is controlled by the Tcl/Tk interpreter. There is no Tcl code involved, however. Bintracker uses [pstk](http://wiki.call-cc.org/eggref/5/pstk) to interface with Tcl/Tk. The relevant GUI code resides in `bt-gui.scm`.

8. While libmdal is mostly [purely functional](https://en.wikipedia.org/wiki/Functional_programming), other parts of Bintracker are not. Most GUI abstractions use the [coops](http://wiki.call-cc.org/eggref/5/coops) object orientation extension, and [closures](https://en.wikipedia.org/wiki/Closure_(computer_programming)) are frequently used to wrap state. The `bt-state` module handles global state in Bintracker.



## Components

### Core Components

#### bintracker-core

`bintracker-core` comprises the main application.

- [Bintracker API](generated/bintracker-core.md)

#### libmdal

Libmdal is an implementation of the [Music Data Abstraction Language](mdal-introduction.md). It covers all parts related to the handling of music modules. Libmdal is a compiler generator: Given a MDEF engine definition, it generates a compiler that can transform a suitable MMOD into binary output.

- [libmdal API](generated/mdal.md)

#### Schemta

[Schemta](schemta.md) is Bintracker's assembler. It is integrated into libmdal but can also be built as a stand-alone executable.

- [Schemta API](generated/schemta.md)

### Dependencies

PEG parsing, comparse/parsec
object-orientation: coops
multithreading model: srfi-18




In addition to the core  and the [libmdal API](generated/mdal.md), the following procedures and APIs are available:

- All Scheme primitives and procedures defined in the [R5RS](https://schemers.org/Documents/Standards/R5RS/) standard.
- The [SRFI-1](https://srfi.schemers.org/srfi-1/srfi-1.html) and [SRFI-13](https://srfi.schemers.org/srfi-13/srfi-13.html) extensions (lists resp. strings).
- The [bitwise](http://wiki.call-cc.org/man/5/Module%20(chicken%20bitwise)) Chicken Scheme module.
- The [pstk](http://wiki.call-cc.org/eggref/5/pstk) Chicken Scheme extension, which offers an interface to the [Tk widget toolkit](https://en.wikipedia.org/wiki/Tk), and the [Tcl programming language](https://en.wikipedia.org/wiki/Tcl).



## Useful Resources

### Scheme Language

- [R5RS](https://schemers.org/Documents/Standards/R5RS/HTML/), the language reference of the Scheme dialect used in Bintracker.

- [The Little Schemer](https://mitpress.mit.edu/books/little-schemer-fourth-edition). A very good starting book on Scheme.

- [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html), aka SICP. This is the "bible" of Scheme programming. Highly recommended if you want to do some serious hacking on Bintracker.

- [The Most Beautiful Program Ever Written](https://www.youtube.com/watch?v=OyfBQmvr2Hc), a talk by William Byrd. Watch if you don't mind getting hooked on Scheme.

### Chicken Scheme

- [Manual](https://wiki.call-cc.org/manual)
- [Wiki](https://wiki.call-cc.org/)
- [Chicken API](https://api.call-cc.org/5/doc/), a search engine for the official Chicken Scheme documentation. Use this as reference for all Scheme and Chicken-related topics.

### Tcl/Tk

- [Tk Command Reference](https://www.tcl-lang.org/man/tcl8.6/TkCmd/contents.htm)
- [Tcl Command Reference](https://www.tcl-lang.org/man/tcl8.6/TclCmd/contents.htm) and  delevop GUI extensions.
- The [pstk git repository](https://github.com/utz82/pstk.git) contains an extensive but somewhat outdated manual of pstk.

### Other

- Bintracker accesses the [MAME Lua Scripting Engine](https://docs.mamedev.org/techspecs/luaengine.html). The online documentation isn' great but the [source code](https://github.com/mamedev/mame/blob/2d1b881c794286f0e090fbc837db1132c6ea042d/src/frontend/mame/luaengine.cpp) documents the Lua engine quite well.
