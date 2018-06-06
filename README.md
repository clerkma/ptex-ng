
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

### Windows/MSVC
First of all, we need to install `bison` ([winflexbison](https://github.com/lexxmark/winflexbison/)) somewhere.

    set YACC=path-to-winflexbison\win_flex_bison-2.5.14\win_bison.exe

#### Compile with nmake

    cd build-msvc
    build-mruby.bat
    build.bat

#### Compile with jom

[Jom](https://wiki.qt.io/Jom)

    cd build-msvc
    build-mruby.bat
    build-jom.bat

## Source

* TeX Live 2018 (`http://tug.org/svn/texlive/trunk/Build/source/`)
* libotf 0.9.16 (`http://cvs.savannah.gnu.org/viewvc/libotf/?root=m17n`)
* libyaml 0.1.7 (`https://github.com/yaml/libyaml`)
* mruby 1.4.1 (`http://mruby.org/downloads/`)

## How to Install

[TeX Wiki/pTeX-ng](https://texwiki.texjp.org/?pTeX-ng)

### Windows (PowerShell)

    tlmgr update --self --all
    Invoke-WebRequest -Uri 'https://texlive.texjp.org/tltexjp-key.asc' -OutFile 'tltexjp-key.asc'
    tlmgr key add tltexjp-key.asc
    tlmgr repository add http://texlive.texjp.org/pretest/tltexjp tltexjp
    tlmgr pinning add tltexjp "*"
    tlmgr install ptex-ng

### macOS

    tlmgr update --self --all
    curl -O https://texlive.texjp.org/tltexjp-key.asc
    tlmgr key add tltexjp-key.asc
    tlmgr repository add http://texlive.texjp.org/pretest/tltexjp tltexjp
    tlmgr pinning add tltexjp '*'
    tlmgr install ptex-ng
    tlmgr path add

### Linux

    tlmgr update --self --all
    wget https://texlive.texjp.org/tltexjp-key.asc
    tlmgr key add tltexjp-key.asc
    tlmgr repository add http://texlive.texjp.org/pretest/tltexjp tltexjp
    tlmgr pinning add tltexjp '*'
    tlmgr install ptex-ng
    tlmgr path add

