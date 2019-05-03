
## Build Status

[![Travis-CI Build Status](https://travis-ci.org/clerkma/ptex-ng.svg?branch=master)](https://travis-ci.org/clerkma/ptex-ng)
[![Appveyor Build Status](https://ci.appveyor.com/api/projects/status/github/clerkma/ptex-ng?branch=master&svg=true)](https://ci.appveyor.com/project/clerkma/ptex-ng)

## How to Build
### Linux/Mac

Run the shell script:

    ./build-gcc/build-aptex.sh

### Windows/MSVC
First of all, we need to install `bison` ([winflexbison](https://github.com/lexxmark/winflexbison/)) somewhere.

    set YACC=path-to-winflexbison\win_flex_bison-2.5.14\win_bison.exe

#### Compile with nmake

    cd build-msvc
    build-mruby.bat
    build-aptex-nmake.bat

#### Compile with jom

[Jom](https://wiki.qt.io/Jom)

    cd build-msvc
    build-mruby.bat
    build-aptex-jom.bat

## Source

* TeX Live 2020/dev (`http://tug.org/svn/texlive/trunk/Build/source/`)
* libotf 0.9.16 (`http://cvs.savannah.gnu.org/viewvc/libotf/?root=m17n`)
* mruby 2.0.1 (`http://mruby.org/downloads/`)

## How to Install

[TeX Wiki/pTeX-ng](https://texwiki.texjp.org/?pTeX-ng)

### Fetch `tltexjp-key.asc`

* Windows, `Invoke-WebRequest -Uri 'https://texlive.texjp.org/tltexjp-key.asc' -OutFile 'tltexjp-key.asc'`
* Linux/macOS, `curl -O https://texlive.texjp.org/tltexjp-key.asc` or `wget https://texlive.texjp.org/tltexjp-key.asc`

### Windows (PowerShell/CMD)

    tlmgr update --self --all
    tlmgr key add tltexjp-key.asc
    tlmgr repository add http://texlive.texjp.org/current/tltexjp tltexjp
    tlmgr pinning add tltexjp "*"
    tlmgr install ptex-ng

### Linux/macOS

    tlmgr update --self --all
    tlmgr key add tltexjp-key.asc
    tlmgr repository add http://texlive.texjp.org/current/tltexjp tltexjp
    tlmgr pinning add tltexjp '*'
    tlmgr install ptex-ng
    tlmgr path add

