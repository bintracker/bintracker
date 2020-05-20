# Setup

## Compiling from Source

Currently the only way to run Bintracker is to build it from the source code.


### Linux

#### Step 1 - Build Dependencies

The following dependencies are required to build Bintracker:

- sqlite3
- Chicken Scheme >=5.0 + extensions
- Mkdocs + Mkdocs-material + Mkdocs-localsearch

First you need to install [Chicken Scheme](https://call-cc.org), version 5.0 or newer.

Chicken is available through most distro package repositories. However, for advanced users we recommend to build it from source and do a user install.

After installing Chicken itself, you need to install the extensions required by Bintracker.

```sh
$ chicken-install comparse coops extras git list-utils matchable pstk scm2wiki simple-exceptions simple-md5 sqlite3 srfi-1 srfi-4 srfi-13 srfi-14 srfi-18 srfi-69 stack test typed-records
```

Note that in order to build the sqlite3 extension, you need an [sqlite3](https://sqlite.org) installation. Your system most likely will have one installed already, but if not, install it through your distro's package manager.

To build the Bintracker documentation, you will need [MkDocs](https://www.mkdocs.org/), the [mkdocs-material](https://github.com/squidfunk/mkdocs-material) theme, and the [mkdocs-localsearch](https://github.com/wilhelmer/mkdocs-localsearch) extension.

```sh
$ pip install --user mkdocs mkdocs-material mkdocs-localsearch==0.5.0
```


#### Step 2 - Compilation

You are now ready to build Bintracker.

```sh
$ cd build/
$ make
```

It should be safe to do a parallel build (`make -jX`), but it will most likely not save a lot of time.

If you are using Emacs and are planning on writing code for Bintracker, you can run make with an additional `ETAGS=1` argument to generate a suitable TAGS file in the main directory.


#### Step 3 - Emulator Setup

If you haven't done so already, install [MAME](https://mamedev.org) and make sure `mame64` is in your search path. If your MAME executable is not named "mame64", edit the file `config/emulators.scm` accordingly.

For most target systems you want to emulate, you will also need to obtain ROM files. Copy ROM files to `roms/MACHINE`, where MACHINE is MAME's target system identifier string. Bintracker itself does not ship ROM files.


### MacOS, BSD, Windows

So far nobody has tried to build Bintracker on any of these platforms. By all means, please try!

Building on MacOS and BSD shouldn't be too hard. The main complication you may run into is that the Makefile currently uses a few bash/GNUmake specific features. Also, the latest MacOS versions apparently do not ship Tcl/Tk anymore, so you will need to install that first.

Building on Windows will be much more difficult, and may require some changes to the source code. You will need either MSYS or MinGW to build Chicken Scheme and Bintracker itself. The mid-term plan is to ship with a [Tclkit](https://tclkits.rkeene.org), so you might try that.

If you succeed at building Bintracker on a non-Linux platform, please get in touch and let us know how you did it.
