# Style Guide

When to the Bintracker source, please follow the rules laid out in this style guide. This will help keep the source neat and readable for everyone.


## General Guidelines

- Put the highest abstractions at the end of the file/section. Define the dependencies of a definition before that definition whenever possible.
- Avoid large procedures. Consider breaking up procedures that exceed 35 lines of code.


## Formatting

- Use a maximum of 80 characters per line.
- Always break after closing parens.
- Follow common formatting guidelines for Scheme. See [SchemeWiki](http://community.schemewiki.org/?scheme-style) for a short overview, or read [Taylor Campbell's guide](http://mumble.net/~campbell/scheme/style.txt) for a more detailed discussion. Note that the Bintracker source generally ignores SchemeWiki's Rule 4. That means it is not necessary to break after each subexpression as long as you break after closing parens.

We recommend using an editor that supports structural editing (aka paredit), such as Emacs, Vim, Sublime Text, VS Code, or Atom. This doesn't just help to avoid syntax errors, but also will ensure reasonable formatting defaults.


## Naming Conventions

- Follow the common Scheme naming conventions, eg.
  - `foo?  ;; this is a predicate`
  - `foo->bar  ;; this is a converter that takes a 'foo and returns a 'bar`
  - `foo-set!  ;; this is a mutator`
- Be verbose. Use descriptive names. Avoid abbreviations, except in cases where the meaning is completely obvious.


## Documentation

We encourage developers to document code extensively in-source.

Bintracker uses [scm2wiki](https://github.com/utz82/scm2wiki) to generate API documentation from its source files. Scm2wiki picks up source comments prefixed with `;;;` (3 semicolons), and adds them to the HTML documentation. If a comment occurs directly before a definition, scm2wiki will automatically prepend a definition header. Document all exported symbols in this way.
