# Setup

## Installation

Currently, binaries are only provided for Windows. Linux and MacOS users need to [compile Bintracker from source](#compiling-from-source).

### Windows

#### Full Package

The full package contains all the required dependencies to run Bintracker, except the ROM files for MAME emulation. Simply extract `bintracker-win64-full.zip` to a directory of your choice. Next, [add ROM files](#adding-rom-files) for all the machines you want to use.

!!! info ""

    Bintracker requires access to the local network in order to communicate with MAME. Depending on your security setup, you may need to manually grant the required permissions.

#### Core Package

The core package contains only Bintracker itself, without the MAME emulator and the Tclkit utility. Use this if you already have MAME installed.

Extract `bintracker-win64-core.zip` to a directory of your choice. Download [Tclkit 8.6 for Windows 64-bit](https://tclkits.rkeene.org/fossil/wiki/Downloads). Move the executable to `bintracker\3rdparty` and rename it to `tclkit.exe`. Now, adjust `bintracker\config\emulators.windows.scm`, where `program-name` is the path to your `mame.exe` and `"roms"` is the path to your MAME ROM directory.

!!! info ""

    Bintracker requires access to the local network in order to communicate with MAME. Depending on your security setup, you may need to manually grant the required permissions.


## Compiling from Source

Currently the only verified way to run Bintracker is to build it from the source code.


### Linux

#### Step 1 - Build Dependencies

The following dependencies are required to build Bintracker:

- sqlite3
- Chicken Scheme >=5.0 + extensions

To build the Bintracker documentation, the following additonal dependencies are required:

- Mkdocs + Mkdocs-material
- scm2wiki

First, obtain the source:

```sh
$ git clone https://github.com/bintracker/bintracker.git
```

Next, install [Chicken Scheme](https://call-cc.org), version 5.0 or newer. Chicken is available through most distro package repositories. However, for advanced users we recommend to build it from source and do a user install.

After installing Chicken itself, you need to install the extensions required by Bintracker.

```sh
$ chicken-install args base64 bitstring comparse coops list-utils matchable pstk s11n simple-md5 sqlite3 stb-image stb-image-write srfi-1 srfi-4 srfi-13 srfi-14 srfi-18 srfi-69 stack test typed-records web-colors
```

Note that in order to build the sqlite3 extension, you need an [sqlite3](https://sqlite.org) installation. Your system most likely will have one installed already, but if not, install it through your distro's package manager.

To build the Bintracker documentation, you will need [scm2wiki](https://github.com/utz82/scm2wiki), [MkDocs](https://www.mkdocs.org/), and the [mkdocs-material](https://github.com/squidfunk/mkdocs-material) theme.

```sh
chicken-install scm2wiki
$ pip install --user mkdocs mkdocs-material
```


#### Step 2 - Compilation

You are now ready to build Bintracker.

```sh
$ cd build/
$ make
```

To build the documentation, run `make docs`.

You can do a parallel build (`make -jX`), however time savings will be minimal. You can `make tests` to run unit tests on libmdal and Schemta. If you are using Emacs and are planning on writing code for Bintracker, you can run make with an additional `ETAGS=1` argument to generate a suitable TAGS file in the main directory.


#### Step 3 - Emulator Setup

If you haven't done so already, install [MAME](https://mamedev.org) and make sure `mame` is in your search path. If your MAME executable is not named "mame", edit the file `config/emulators.scm` accordingly.

For most target systems you want to emulate, you will also need to obtain ROM files. See [Adding ROM Files](#adding-rom-files) for details on how to add ROMs to MAME for use in Bintracker.

Once you've completed these steps, you can run the `bintracker` executable in the `build` directory.

!!! tip ""

    If you notice sound being choppy, you can try adding `"-nofilter" "-nomax"` and/or `"-autoframeskip"` to the list of MAME default-args in `config/emulators.scm`. Especially hard cases may be fixed with `"-video" "none"`. Newer versions of MAME will complain about the latter, but it nevertheless fixes most cases of bad audio.


### Windows

Building Bintracker on Windows is rather messy, unfortunately. While there are several ways in which this could be done, we recommend building with [MSYS2](https://www.msys2.org) and [Chocolatey](https://chocolatey.org/).

If you manage to build Bintracker on Windows by other means, please let us know how you did it, either by opening a [Github Issue](https://github.com/bintracker/bintracker/issues) or by [sending a message](https://bintracker.org/contact/).

#### Step 1 - Set up the build system

First, follow the instructions at https://chocolatey.org/install to set up Chocolatey. Next, in an admin Powershell, run

```
> choco install chicken
```

This will set up MSYS and MinGW (if you don't have it set up already), and install Chicken Scheme 5.2.0 in `C:\tools\chicken`. Next, install GMake with

```
> choco install make
```

#### Step 2 - Install Scheme libraries

Launch `C:\tools\msys64\mingw64.exe` with administrator rights. First, install SQLite with

```
% pacman -S mingw-w64-x86_64-sqlite3
```

Next, install the srfi-18 egg with

```
% /c/tools/chicken/bin/chicken-install.exe srfi-18
```

Running this the first time will fail because of a problem with the build script. In `C:\Users\<your-username>\AppData\Local\chicken-install\srfi-18\build-srfi-18.bat`, replace `%CHICKEN_CSI%` with `csi` and `%CHICKEN_CSC%` with `csc`. Then, run the above install command again.

Next, install the check-errors egg with

```
% /c/tools/chicken/bin/chicken-install.exe check-errors
```

This will likely crash near the end, breaking `chicken-install` in the process. To fix this, create a file named `check-errors.egg-info` in `C:\tools\chicken\lib\chicken\11`, and paste the following contents into it:

```
((installed-files
   "C:/tools/chicken/lib/chicken/11/check-errors.sys.obj"
   "C:/tools/chicken/lib/chicken/11/check-errors.sys.link"
   "C:/tools/chicken/lib/chicken/11/check-errors.sys.so"
   "C:/tools/chicken/lib/chicken/11/check-errors.sys.types"
   "C:/tools/chicken/lib/chicken/11/check-errors.sys.inline"
   "C:/tools/chicken/lib/chicken/11/check-errors.sys.import.so"
   "C:/tools/chicken/lib/chicken/11/check-errors.obj"
   "C:/tools/chicken/lib/chicken/11/check-errors.link"
   "C:/tools/chicken/lib/chicken/11/check-errors.so"
   "C:/tools/chicken/lib/chicken/11/check-errors.types"
   "C:/tools/chicken/lib/chicken/11/check-errors.import.so"
   "C:/tools/chicken/lib/chicken/11/type-checks.obj"
   "C:/tools/chicken/lib/chicken/11/type-checks.link"
   "C:/tools/chicken/lib/chicken/11/type-checks.so"
   "C:/tools/chicken/lib/chicken/11/type-checks.types"
   "C:/tools/chicken/lib/chicken/11/type-checks.import.so"
   "C:/tools/chicken/lib/chicken/11/type-errors.obj"
   "C:/tools/chicken/lib/chicken/11/type-errors.link"
   "C:/tools/chicken/lib/chicken/11/type-errors.so"
   "C:/tools/chicken/lib/chicken/11/type-errors.types"
   "C:/tools/chicken/lib/chicken/11/type-errors.import.so"
   "C:/tools/chicken/lib/chicken/11/check-errors.basic.obj"
   "C:/tools/chicken/lib/chicken/11/check-errors.basic.link"
   "C:/tools/chicken/lib/chicken/11/check-errors.basic.so"
   "C:/tools/chicken/lib/chicken/11/check-errors.basic.types"
   "C:/tools/chicken/lib/chicken/11/check-errors.basic.import.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-basic.obj"
   "C:/tools/chicken/lib/chicken/11/type-checks-basic.link"
   "C:/tools/chicken/lib/chicken/11/type-checks-basic.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-basic.types"
   "C:/tools/chicken/lib/chicken/11/type-checks-basic.import.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-basic.obj"
   "C:/tools/chicken/lib/chicken/11/type-errors-basic.link"
   "C:/tools/chicken/lib/chicken/11/type-errors-basic.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-basic.types"
   "C:/tools/chicken/lib/chicken/11/type-errors-basic.import.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-atoms.obj"
   "C:/tools/chicken/lib/chicken/11/type-checks-atoms.link"
   "C:/tools/chicken/lib/chicken/11/type-checks-atoms.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-atoms.types"
   "C:/tools/chicken/lib/chicken/11/type-checks-atoms.import.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-atoms.obj"
   "C:/tools/chicken/lib/chicken/11/type-errors-atoms.link"
   "C:/tools/chicken/lib/chicken/11/type-errors-atoms.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-atoms.types"
   "C:/tools/chicken/lib/chicken/11/type-errors-atoms.import.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.obj"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.link"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.types"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.import.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.obj"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.link"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.types"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.import.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.interval.obj"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.interval.link"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.interval.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.interval.types"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.interval.import.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.interval.obj"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.interval.link"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.interval.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.interval.types"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.interval.import.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.scheme.obj"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.scheme.link"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.scheme.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.scheme.types"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.scheme.import.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.scheme.obj"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.scheme.link"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.scheme.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.scheme.types"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.scheme.import.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.number.obj"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.number.link"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.number.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.number.types"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.number.import.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.number.obj"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.number.link"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.number.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.number.types"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.number.import.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.fixnum.obj"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.fixnum.link"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.fixnum.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.fixnum.types"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.fixnum.import.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.fixnum.obj"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.fixnum.link"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.fixnum.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.fixnum.types"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.fixnum.import.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.integer.obj"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.integer.link"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.integer.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.integer.types"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.integer.import.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.integer.obj"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.integer.link"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.integer.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.integer.types"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.integer.import.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.bignum.obj"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.bignum.link"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.bignum.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.bignum.types"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.bignum.import.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.bignum.obj"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.bignum.link"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.bignum.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.bignum.types"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.bignum.import.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.ratnum.obj"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.ratnum.link"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.ratnum.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.ratnum.types"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.ratnum.import.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.ratnum.obj"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.ratnum.link"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.ratnum.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.ratnum.types"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.ratnum.import.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.flonum.obj"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.flonum.link"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.flonum.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.flonum.types"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.flonum.import.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.flonum.obj"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.flonum.link"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.flonum.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.flonum.types"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.flonum.import.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.cplxnum.obj"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.cplxnum.link"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.cplxnum.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.cplxnum.types"
   "C:/tools/chicken/lib/chicken/11/type-checks-numbers.cplxnum.import.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.cplxnum.obj"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.cplxnum.link"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.cplxnum.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.cplxnum.types"
   "C:/tools/chicken/lib/chicken/11/type-errors-numbers.cplxnum.import.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-structured.obj"
   "C:/tools/chicken/lib/chicken/11/type-checks-structured.link"
   "C:/tools/chicken/lib/chicken/11/type-checks-structured.so"
   "C:/tools/chicken/lib/chicken/11/type-checks-structured.types"
   "C:/tools/chicken/lib/chicken/11/type-checks-structured.import.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-structured.obj"
   "C:/tools/chicken/lib/chicken/11/type-errors-structured.link"
   "C:/tools/chicken/lib/chicken/11/type-errors-structured.so"
   "C:/tools/chicken/lib/chicken/11/type-errors-structured.types"
   "C:/tools/chicken/lib/chicken/11/type-errors-structured.import.so"
   "C:/tools/chicken/lib/chicken/11/srfi-4-checks.obj"
   "C:/tools/chicken/lib/chicken/11/srfi-4-checks.link"
   "C:/tools/chicken/lib/chicken/11/srfi-4-checks.so"
   "C:/tools/chicken/lib/chicken/11/srfi-4-checks.types"
   "C:/tools/chicken/lib/chicken/11/srfi-4-checks.import.so"
   "C:/tools/chicken/lib/chicken/11/srfi-4-errors.obj"
   "C:/tools/chicken/lib/chicken/11/srfi-4-errors.link"
   "C:/tools/chicken/lib/chicken/11/srfi-4-errors.so"
   "C:/tools/chicken/lib/chicken/11/srfi-4-errors.types"
   "C:/tools/chicken/lib/chicken/11/srfi-4-errors.import.so")
 (version "3.6.1")
 (synopsis "Argument checks & errors")
 (version "3.6.1")
 (category misc)
 (license "BSD")
 (author "Kon Lovett")
 (test-dependencies test)
 (component-options
   (csc-options
     "-O3"
     "-d1"
     "-strict-types"
     "-no-procedure-checks"
     "-no-bound-checks"))
 (components
   (extension check-errors.sys (types-file) (inline-file))
   (extension
     check-errors
     (types-file)
     (component-dependencies type-checks srfi-4-checks))
   (extension
     type-checks
     (types-file)
     (component-dependencies
       type-errors
       type-checks-basic
       type-checks-atoms
       type-checks-structured))
   (extension
     type-errors
     (types-file)
     (component-dependencies
       type-errors-basic
       type-errors-atoms
       type-errors-structured))
   (extension
     check-errors.basic
     (types-file)
     (component-dependencies type-checks-basic type-errors-basic))
   (extension
     type-checks-basic
     (types-file)
     (component-dependencies type-errors-basic))
   (extension type-errors-basic (types-file))
   (extension
     type-checks-atoms
     (types-file)
     (component-dependencies
       type-checks-basic
       type-checks-numbers
       type-errors-atoms))
   (extension
     type-errors-atoms
     (types-file)
     (component-dependencies type-errors-basic type-errors-numbers))
   (extension
     type-checks-numbers
     (types-file)
     (component-dependencies
       type-checks-basic
       type-errors-numbers
       type-checks-numbers.interval
       type-checks-numbers.scheme
       type-checks-numbers.number
       type-checks-numbers.fixnum
       type-checks-numbers.integer
       type-checks-numbers.bignum
       type-checks-numbers.ratnum
       type-checks-numbers.flonum
       type-checks-numbers.cplxnum))
   (extension
     type-errors-numbers
     (types-file)
     (component-dependencies
       type-errors-basic
       type-errors-numbers.interval
       type-errors-numbers.scheme
       type-errors-numbers.number
       type-errors-numbers.fixnum
       type-errors-numbers.integer
       type-errors-numbers.bignum
       type-errors-numbers.ratnum
       type-errors-numbers.flonum
       type-errors-numbers.cplxnum))
   (extension
     type-checks-numbers.interval
     (types-file)
     (component-dependencies type-checks-basic type-errors-numbers.interval))
   (extension
     type-errors-numbers.interval
     (types-file)
     (component-dependencies type-errors-basic))
   (extension
     type-checks-numbers.scheme
     (types-file)
     (component-dependencies type-checks-basic type-errors-numbers.scheme))
   (extension
     type-errors-numbers.scheme
     (types-file)
     (component-dependencies type-errors-basic))
   (extension
     type-checks-numbers.number
     (types-file)
     (component-dependencies type-checks-basic type-errors-numbers.number))
   (extension
     type-errors-numbers.number
     (types-file)
     (component-dependencies type-errors-basic))
   (extension
     type-checks-numbers.fixnum
     (types-file)
     (component-dependencies type-checks-basic type-errors-numbers.fixnum))
   (extension
     type-errors-numbers.fixnum
     (types-file)
     (component-dependencies type-errors-basic))
   (extension
     type-checks-numbers.integer
     (types-file)
     (component-dependencies type-checks-basic type-errors-numbers.integer))
   (extension
     type-errors-numbers.integer
     (types-file)
     (component-dependencies type-errors-basic))
   (extension
     type-checks-numbers.bignum
     (types-file)
     (component-dependencies type-checks-basic type-errors-numbers.bignum))
   (extension
     type-errors-numbers.bignum
     (types-file)
     (component-dependencies type-errors-basic))
   (extension
     type-checks-numbers.ratnum
     (types-file)
     (component-dependencies type-checks-basic type-errors-numbers.ratnum))
   (extension
     type-errors-numbers.ratnum
     (types-file)
     (component-dependencies type-errors-basic))
   (extension
     type-checks-numbers.flonum
     (types-file)
     (component-dependencies type-checks-basic type-errors-numbers.flonum))
   (extension
     type-errors-numbers.flonum
     (types-file)
     (component-dependencies type-errors-basic))
   (extension
     type-checks-numbers.cplxnum
     (types-file)
     (component-dependencies type-checks-basic type-errors-numbers.cplxnum))
   (extension
     type-errors-numbers.cplxnum
     (types-file)
     (component-dependencies type-errors-basic))
   (extension
     type-checks-structured
     (types-file)
     (component-dependencies type-checks-basic type-errors-structured))
   (extension
     type-errors-structured
     (types-file)
     (component-dependencies type-errors-basic))
   (extension
     srfi-4-checks
     (types-file)
     (component-dependencies type-checks-basic srfi-4-errors))
   (extension
     srfi-4-errors
     (types-file)
     (component-dependencies type-errors-basic))))
```

Now, install the remaining dependencies with

```
% /c/tools/chicken/bin/chicken-install.exe args base64 bitstring comparse coops list-utils matchable pstk s11n simple-md5 sqlite3 stb-image stb-image-write srfi-1 srfi-4 srfi-13 srfi-14 srfi-69 shell stack test typed-records web-colors
```


#### Step 3 - Get source code and runtime dependencies

Download the Bintracker [source code](https://github.com/bintracker/bintracker/archive/refs/heads/master.zip) and unpack it to a directory of your choice. Next, download Tclkit 8.6 for Windows 64-bit from https://tclkits.rkeene.org/fossil/wiki/Downloads or generate an appropriate kit that includes the Tk package on https://kitcreator.rkeene.org/kitcreator . Move the executable to `bintracker\3rdparty` and rename it to `tclkit.exe`. Then, download the latest MAME release from https://github.com/mamedev/mame/releases and unpack the self-extracting archive to `bintracker\3rdparty\mame`.


#### Step 4 - Build Bintracker

In a Powershell, navigate to `bintracker\build`, and run

```
> make -f .\Makefile.msys
```

Finally, copy `C:\tools\msys64\usr\bin\msys-2.0.dll` and `C:\tools\msys64\usr\bin\libsqlite3-0.dll` to `bintracker\build` in order to package an executable release from the build directory. That's all. Building the documentation on Windows is untested, but should work in the same way as on Linux. Otherwise just use the [online documentation](https://bintracker.org/documentation) instead.


### MacOS, BSD

So far nobody has tried to build Bintracker on any of these platforms. By all means, please try!

Building on MacOS and BSD shouldn't be too hard. The main complication you may run into is that the Makefile currently uses a few bash/GNUmake specific features. Also, the latest MacOS versions apparently do not ship Tcl/Tk anymore, so you will need to install that first.

If you succeed at building Bintracker on a non-Linux platform, please let us know how you did it, either by opening a [Github Issue](https://github.com/bintracker/bintracker/issues) or by [sending a message](https://bintracker.org/contact/).


## Adding ROM Files

For legal reasons, Bintracker does not ship any of the ROM files required by MAME. They are usually easy to come by on the web, though (pro-tip: [archive.org](https://archive.org) is your friend). The tricky part is figuring out exactly which files MAME expects. The [Arcade Database](http://adb.arcadeitalia.net/) lists these details for every system supported in MAME (use the search to find your system, then check the section *Files* -> *Romset*).

ROM files go into `bintracker/roms/*`, or whatever you specified as your ROM path in `config/emulators[.windows].scm`. Create a subfolder for each machine, named exactly as the machine is named on MAME. The Arcade Database also list the official names. The systems currently supported in Bintracker use the following names:

| System                   | MAME Name | Remarks               |
|--------------------------|-----------|-----------------------|
| Atari 2600/VCS           | a2600     | requires no ROM files |
| Dragon 32                | dragon32  |                       |
| Exidy Sorcerer           | sorcerer  |                       |
| Fairchild Channel F      | channelf  |                       |
| Sinclair ZX Spectrum 48K | spectrum  |                       |
| TRS-80 MC-10             | mc10      |                       |
| Tandy Color Computer 3   | coco3     |                       |
