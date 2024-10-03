
## Build Status

![pTeX-ng Workflow](https://github.com/clerkma/ptex-ng/actions/workflows/build-bin.yml/badge.svg)
[![Appveyor Build Status](https://ci.appveyor.com/api/projects/status/github/clerkma/ptex-ng?branch=master&svg=true)](https://ci.appveyor.com/project/clerkma/ptex-ng)

## How to Build
### Linux/Mac

Run the shell script:

    ./build-gcc/build-aptex.sh

### Windows/MSVC

#### Compile with nmake

    cd build-msvc
    build-mruby.bat
    build-aptex.bat

#### Compile with jom

[Jom](https://wiki.qt.io/Jom)

    cd build-msvc
    build-mruby.bat
    build-aptex.bat jom

## Source

* `texlive`: TeX Live 2025/dev (`http://tug.org/svn/texlive/trunk/Build/source/`)
  * `svn co svn://tug.org/texlive/trunk/Build/source texlive`

