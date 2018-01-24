
## Build Status

[![Travis-CI Build Status](https://travis-ci.org/clerkma/ptex-ng.svg?branch=master)](https://travis-ci.org/clerkma/ptex-ng)
[![Appveyor Build Status](https://ci.appveyor.com/api/projects/status/github/clerkma/ptex-ng?branch=master&svg=true)](https://ci.appveyor.com/project/clerkma/ptex-ng)

## How to Build
### Linux/Mac

Run the shell script:

    ./build-aptex.sh

### Android

    cd build-android
    ./build-android-port.sh

### Windows/MSVC (experimental)
#### nmake

    cd build-msvc
    build.bat

#### jom

[Jom](https://wiki.qt.io/Jom)

    cd build-msvc
    build-jom.bat

## Source

* TeX Live 2017 (`http://tug.org/svn/texlive/trunk/Build/source/`)
* libotf 0.9.16 (`http://cvs.savannah.gnu.org/viewvc/libotf/?root=m17n`)
* libyaml 0.1.7 (`https://github.com/yaml/libyaml`)
* mruby 1.3.0 (`http://mruby.org/downloads/`)

## How to Install

[TeX Wiki/pTeX-ng](https://texwiki.texjp.org/?pTeX-ng)
