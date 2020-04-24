# Getting Started with Bintracker Development


## Prerequisites

As Bintracker is written in the [Scheme programming language](https://en.wikipedia.org/wiki/Scheme_(programming_language)), you will need some understanding of Scheme or another Lisp-like language. Although Scheme's syntax is very different from other programming languages, it is very easy to learn. Even a basic understanding is sufficient to start running some simple code in Bintracker.

If you plan on writing more than a few lines of code for Bintracker, you should consider using an editor with a structured editing mode. Vim, Atom, and VS Code all support something akin to Emacs' [paredit](http://danmidwood.com/content/2014/11/21/animated-paredit.html).


## Useful Resources

### Scheme

- [R5RS](https://schemers.org/Documents/Standards/R5RS/HTML/), the language reference of the Scheme dialect used in Bintracker.

- [The Little Schemer](https://mitpress.mit.edu/books/little-schemer-fourth-edition). A very good starting book on Scheme.

- [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html), aka SICP. This is the "bible" of Scheme programming. Highly recommended if you want to do some serious hacking on Bintracker.

- [The Most Beautiful Program Ever Written](https://www.youtube.com/watch?v=OyfBQmvr2Hc), a talk by William Byrd. Watch if you don't mind getting hooked on Scheme.

### Chicken Scheme

- [Manual](https://wiki.call-cc.org/manual)
- [Wiki](https://wiki.call-cc.org/)
- [Chicken API](https://api.call-cc.org/5/doc/), a search engine for the official Chicken Scheme documentation. Use this as reference for all Scheme and Chicken-related topics.

### Miscellaneous

- The [Tcl](https://www.tcl-lang.org/man/tcl8.6/TclCmd/contents.htm) and [Tk](https://www.tcl-lang.org/man/tcl8.6/TkCmd/contents.htm) references. You may need these if you want to delevop GUI extensions. Bintracker talks to Tcl/Tk via pstk, which has an extensive manual in the [pstk git repository](https://github.com/utz82/pstk.git).
- Bintracker access the [MAME Scripting Engine](https://docs.mamedev.org/techspecs/luaengine.html). The online documentation isn' great but the [source code](https://github.com/mamedev/mame/blob/2d1b881c794286f0e090fbc837db1132c6ea042d/src/frontend/mame/luaengine.cpp) documents the Lua engine quite well.

## Where to Start


## Components

### Core Components

### Dependencies


In addition to the core [Bintracker API](generated/bintracker-core.md) and the [libmdal API](libmdal/generated/mdal.md), the following procedures and APIs are available:

- All Scheme primitives and procedures defined in the [R5RS](https://schemers.org/Documents/Standards/R5RS/) standard.
- The [SRFI-1](https://srfi.schemers.org/srfi-1/srfi-1.html) and [SRFI-13](https://srfi.schemers.org/srfi-13/srfi-13.html) extensions (lists resp. strings).
- The [bitwise](http://wiki.call-cc.org/man/5/Module%20(chicken%20bitwise)) Chicken Scheme module.
- The [pstk](http://wiki.call-cc.org/eggref/5/pstk) Chicken Scheme extension, which offers an interface to the [Tk widget toolkit](https://en.wikipedia.org/wiki/Tk), and the [Tcl programming language](https://en.wikipedia.org/wiki/Tcl).
