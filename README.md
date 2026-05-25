
## Build Status

![Build Binaries](https://github.com/clerkma/ptex-ng/actions/workflows/blade-build-bin.yml/badge.svg)

## How to Build

### Linux/macOS (blade-build)

Prerequisites: `automake`, `autoconf`, `libtool`, `texinfo`, `ninja`, `gcc`/`clang`

Install [blade-build](https://github.com/blade-build/blade-build) (v2@`3295898226ee43f01f3b9c7081d6f24d7f4556b7`):

    bash task/install-blade-v2.sh

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
