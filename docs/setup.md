# Setup


## Compiling from Source

## Windows


## Linux, *BSD

First, install the the tools necessary to build Bintracker.

- [MkDocs](https://www.mkdocs.org/) - Most likely available from your distro package manager.

- [Chicken Scheme](https://call-cc.org) - version 5.0 or newer.

Chicken is available through most distro package repositories, though technically versed users may instead wish it from source and do a user install.

After installing Chicken itself, you need to install the extensions required by Bintracker.

```sh
$ chicken-install comparse extras git list-utils markdown-svnwiki matchable pstk simple-exceptions simple-md5 srfi-1 srfi-4 srfi-13 srfi-14 srfi-69 stack test typed-records
```

- [scm2wiki](https://github.com/utz82/scm2wiki)

```sh
$ git clone https://github.com/utz82/scm2wiki.git
$ cd scm2wiki
$ make && make docs
```

Afterwards, copy the resulting binary to your executable directory, eg. ```$ sudo cp scm2wiki /usr/bin```

You are now ready to build Bintracker by running `make` in the Bintracker directory.

## Mac OS X

...
