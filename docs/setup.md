# Setup

## Compiling from Source

Currently the only way to run Bintracker is to build it from the source code.


### Windows

...


### Linux, *BSD

#### Step 1 - Build Dependencies

First you need to install [Chicken Scheme](https://call-cc.org), version 5.0 or newer.

Chicken is available through most distro package repositories. However, for advanced users we recommend to build it from source and do a user install.

After installing Chicken itself, you need to install the extensions required by Bintracker.

```sh
$ chicken-install comparse coops extras git list-utils matchable pstk scm2wiki simple-exceptions simple-md5 sql-de-lite srfi-1 srfi-4 srfi-13 srfi-14 srfi-18 srfi-69 stack test typed-records
```

To build the Bintracker documentation, you will need [MkDocs](https://www.mkdocs.org/), the [mkdocs-material](https://github.com/squidfunk/mkdocs-material) theme, and the [mkdocs-localsearch](https://github.com/wilhelmer/mkdocs-localsearch) extension.

```sh
$ pip install --user mkdocs mkdocs-material mkdocs-localsearch==0.5.0
```

#### Step 2 - Compilation

You are now ready to build Bintracker by running `make && make-docs` in the Bintracker directory.


#### Step 3 - Emulator Setup

If you haven't done so already, install [MAME](https://mamedev.org) and make sure `mame64` is in your search path. If your MAME executable is not named "mame64", edit the file `config/emulators.scm` accordingly.

For most target systems you want to emulate, you will also need to obtain ROM files. Copy ROM files to `roms/MACHINE`, where MACHINE is MAME's target system identifier string. Bintracker itself does not ship ROM files.

### Mac OS X

...
