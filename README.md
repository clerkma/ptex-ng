How to build ptex-ng:

* set a variable:
```
cd ptex-ng
export NG_SRC_DIR=`pwd`
```
* zlib
```
cd $NG_SRC_DIR/libs/zlib && ./configure && make
cp libz.a $NG_SRC_DIR/texk/ptex-ng
```

* libpng
```
cd $NG_SRC_DIR/libs/libpng && ./configure && make
cp libpng.a $NG_SRC_DIR/texk/ptex-ng
```

* kpathsea
```
cd $NG_SRC_DIR/texk/kpathsea && ./configure && make
cp .libs/libkpathsea.a $NG_SRC_DIR/texk/ptex-ng
```

* ptexenc
```
cd $NG_SRC_DIR/texk/ptexenc && ./configure && make
cp .libs/libptexenc.a $NG_SRC_DIR/texk/ptex-ng
```

* libdpx
```
cd $NG_SRC_DIR/texk/libdpx && ./configure && make
cp libdpx.a $NG_SRC_DIR/texk/ptex-ng
```

* ptex-ng
```
cd $NG_SRC_DIR/texk/ptex-ng && make
```