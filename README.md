
## Build Status

![Build Binaries](https://github.com/Liam0205/ptex-ng/actions/workflows/blade-build-bin.yml/badge.svg)

## How to Build

### Linux/macOS (blade-build)

Prerequisites: `automake`, `autoconf`, `libtool`, `texinfo`, `ninja`, `gcc`/`clang`

Install [blade-build](https://github.com/blade-build/blade-build):

    curl https://blade-build.github.io/install.sh | bash

Build:

    blade build //src:aptex

Output binary: `blade-bin/src/aptex`

### Windows/MSVC

#### Compile with nmake

    cd build-msvc
    build-aptex.bat

#### Compile with jom

[Jom](https://wiki.qt.io/Jom)

    cd build-msvc
    build-aptex.bat jom

## Source

* `texlive`: TeX Live 2027/dev (`http://tug.org/svn/texlive/trunk/Build/source/`)
  * `svn co svn://tug.org/texlive/trunk/Build/source texlive`
