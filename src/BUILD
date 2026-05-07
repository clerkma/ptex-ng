# TeX Live dependency libraries (built via autoconf)

gen_rule(
    name = 'zlib_build',
    outs = ['lib/libz.a'],
    cmd = 'mkdir -p $OUT_DIR/lib && (cd texlive/libs/zlib && (test -f Makefile || ./configure) && make -j) && cp texlive/libs/zlib/libz.a $FIRST_OUT',
    heavy = True,
)

gen_rule(
    name = 'libpng_build',
    deps = [':zlib_build'],
    outs = ['lib/libpng.a'],
    cmd = 'mkdir -p $OUT_DIR/lib && (cd texlive/libs/libpng && (test -f Makefile || ./configure) && make -j) && cp texlive/libs/libpng/libpng.a $FIRST_OUT',
    heavy = True,
)

gen_rule(
    name = 'libpaper_build',
    outs = ['lib/libpaper.a'],
    cmd = 'mkdir -p $OUT_DIR/lib && (cd texlive/libs/libpaper && (test -f Makefile || ./configure) && make -j) && cp texlive/libs/libpaper/libpaper.a $FIRST_OUT',
    heavy = True,
)

gen_rule(
    name = 'kpathsea_build',
    outs = ['lib/libkpathsea.a'],
    cmd = 'mkdir -p $OUT_DIR/lib && (cd texlive/texk/kpathsea && (test -f Makefile || ./configure) && make -j) && cp texlive/texk/kpathsea/.libs/libkpathsea.a $FIRST_OUT',
    heavy = True,
)

gen_rule(
    name = 'ptexenc_build',
    deps = [':kpathsea_build'],
    outs = ['lib/libptexenc.a'],
    cmd = 'mkdir -p $OUT_DIR/lib && (cd texlive/texk/ptexenc && (test -f Makefile || ./configure) && make -j) && cp texlive/texk/ptexenc/.libs/libptexenc.a $FIRST_OUT',
    heavy = True,
)

gen_rule(
    name = 'libdpx_build',
    deps = [':zlib_build', ':libpng_build', ':libpaper_build', ':kpathsea_build'],
    outs = ['lib/libdpx.a'],
    cmd = 'mkdir -p $OUT_DIR/lib && TLROOT=$$(pwd)/texlive make -C src/libdpx -j && cp src/libdpx/libdpx.a $FIRST_OUT',
)

gen_rule(
    name = 'libw2c_build',
    deps = [':kpathsea_build', ':ptexenc_build', ':zlib_build'],
    outs = ['lib/libw2c.a'],
    cmd = 'mkdir -p $OUT_DIR/lib && TLROOT=$$(pwd)/texlive make -C src/libw2c -j && cp src/libw2c/libw2c.a $FIRST_OUT',
)

# Wrap gen_rule outputs as linkable libraries

foreign_cc_library(
    name = 'z',
    install_dir = '',
    deps = [':zlib_build'],
)

foreign_cc_library(
    name = 'png',
    install_dir = '',
    deps = [':libpng_build'],
)

foreign_cc_library(
    name = 'paper',
    install_dir = '',
    deps = [':libpaper_build'],
)

foreign_cc_library(
    name = 'kpathsea',
    install_dir = '',
    deps = [':kpathsea_build'],
)

foreign_cc_library(
    name = 'ptexenc',
    install_dir = '',
    deps = [':ptexenc_build'],
)

foreign_cc_library(
    name = 'dpx',
    install_dir = '',
    deps = [':libdpx_build', ':png', ':paper', ':z', ':kpathsea'],
)

foreign_cc_library(
    name = 'w2c',
    install_dir = '',
    deps = [':libw2c_build', ':z', ':kpathsea', ':ptexenc'],
)

# Main engine binary

cc_binary(
    name = 'aptex',
    srcs = [
        'aptex.c',
        'aptex-src.c',
        'aptex-utils.c',
    ],
    deps = [
        ':z',
        ':png',
        ':paper',
        ':kpathsea',
        ':ptexenc',
        ':dpx',
        ':w2c',
        '#m',
    ],
    incs = [
        '.',
        '../texlive/texk',
        '../texlive/texk/ptexenc',
        '../texlive/libs/zlib/include',
        '../texlive/texk/web2c/libmd5',
    ],
    extra_cppflags = ['-Werror'],
    warning = 'no',
)
