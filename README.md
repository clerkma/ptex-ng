
## Build Status

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

* TeX Live 2024/dev (`http://tug.org/svn/texlive/trunk/Build/source/`)
* libotf 0.9.16 (`http://cvs.savannah.gnu.org/viewvc/libotf/?root=m17n`)
* mruby 3.2.0 (`http://mruby.org/downloads/`)

## How to Install

Author's testing repository.

#### Installation on Windows/Linux/macOS

    tlmgr update --self --all
    tlmgr repository add https://www.hemibit.com/2021 ptexngdist
    tlmgr pinning add ptexngdist "*"
    tlmgr install ptex-ng
    tlmgr path add

