# Setup

## Compiling from Source

Currently the only verified way to run Bintracker is to build it from the source code.


### Linux

#### Step 1 - Build Dependencies

The following dependencies are required to build Bintracker:

- sqlite3
- Chicken Scheme >=5.0 + extensions

To build the Bintracker documentation, the following additonal dependencies are required:

- Mkdocs + Mkdocs-material + Mkdocs-localsearch
- scm2wiki

First, obtain the source:

```sh
$ git clone https://github.com/bintracker/bintracker.git
```

Next,install [Chicken Scheme](https://call-cc.org), version 5.0 or newer. Chicken is available through most distro package repositories. However, for advanced users we recommend to build it from source and do a user install.

After installing Chicken itself, you need to install the extensions required by Bintracker.

```sh
$ chicken-install args base64 bitstring comparse coops list-utils matchable pstk simple-md5 sqlite3 stb-image stb-image-write srfi-1 srfi-4 srfi-13 srfi-14 srfi-18 srfi-69 stack test typed-records web-colors
```

Note that in order to build the sqlite3 extension, you need an [sqlite3](https://sqlite.org) installation. Your system most likely will have one installed already, but if not, install it through your distro's package manager.

To build the Bintracker documentation, you will need [scm2wiki](https://github.com/utz82/scm2wiki), [MkDocs](https://www.mkdocs.org/), the [mkdocs-material](https://github.com/squidfunk/mkdocs-material) theme, and the [mkdocs-localsearch](https://github.com/wilhelmer/mkdocs-localsearch) extension.

```sh
git clone https://github.com/utz82/scm2wiki.git
cd scm2wiki
chicken-install
$ pip install --user mkdocs mkdocs-material mkdocs-localsearch==0.5.0
```


#### Step 2 - Compilation

You are now ready to build Bintracker.

```sh
$ cd build/
$ make
```

You can do a parallel build (`make -jX`), however time savings will be minimal. You can `make tests` to run unit tests on libmdal and Schemta. If you are using Emacs and are planning on writing code for Bintracker, you can run make with an additional `ETAGS=1` argument to generate a suitable TAGS file in the main directory.


#### Step 3 - Emulator Setup

If you haven't done so already, install [MAME](https://mamedev.org) and make sure `mame64` is in your search path. If your MAME executable is not named "mame64", edit the file `config/emulators.scm` accordingly.

For most target systems you want to emulate, you will also need to obtain ROM files. Bintracker itself does not ship ROM files except for a handful of open source replacement ROMs. Copy ROM files to `roms/MACHINE/`, where MACHINE is MAME's target system identifier string. MAME is very peculiar about the names of ROM files. The [Arcade Database](http://adb.arcadeitalia.net/) is a good source of information regarding this.

Once you've completed these steps, you can run the `bintracker` executable in the `build` directory.

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

Now, install the remaining dependencies with

```
% /c/tools/chicken/bin/chicken-install.exe args base64 bitstring comparse coops list-utils matchable pstk simple-md5 sqlite3 stb-image stb-image-write srfi-1 srfi-4 srfi-13 srfi-14 srfi-69 shell stack test typed-records web-colors
```

#### Step 3 - Get source code and runtime dependencies

Download the Bintracker [source code](https://github.com/bintracker/bintracker/archive/refs/heads/master.zip) and unpack it to a directory of your choice. Next, download Tclkit 8.6 for Windows 64-bit from https://tclkits.rkeene.org/fossil/wiki/Downloads . Move the executable to `bintracker\3rdparty` and rename it to `tclkit.exe`. Then, download the latest MAME release from https://github.com/mamedev/mame/releases and unpack the self-extracting archive to `bintracker\3rdparty\mame`.

#### Step 4 - Build Bintracker

In a Powershell, navigate to `bintracker\build`, and run

```
> make -f .\Makefile.msys
```

Finally, copy `C:\tools\msys64\usr\bin\msys-2.0.dll` to `bintracker\build`. That's all. Unfortunately, building the documentation on Windows is not supported, just use the [online documentation](https://bintracker.org/documentation) instead.


### MacOS, BSD

So far nobody has tried to build Bintracker on any of these platforms. By all means, please try!

Building on MacOS and BSD shouldn't be too hard. The main complication you may run into is that the Makefile currently uses a few bash/GNUmake specific features. Also, the latest MacOS versions apparently do not ship Tcl/Tk anymore, so you will need to install that first.

If you succeed at building Bintracker on a non-Linux platform, please please let us know how you did it, either by opening a [Github Issue](https://github.com/bintracker/bintracker/issues) or by [sending a message](https://bintracker.org/contact/).
