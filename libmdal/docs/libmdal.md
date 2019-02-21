# About libmdal

**libmdal** is a library for handling MDAL modules and configurations. It is
provided as a module for the [Chicken 4](https://call-cc.org/) implementation of
the Scheme programming language.

libmdal is internally split up into several submodules. However, all API
functionality is provided through the `mdal` meta-module. To use libmdal in your
program, simply `use` the library with

```scheme
(use mdal)
```
