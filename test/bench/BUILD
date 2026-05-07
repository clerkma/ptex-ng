# Benchmark targets

cc_binary(
    name = 'bench_engine',
    srcs = ['harness/bench_engine.c'],
    deps = ['#m'],
)

sh_test(
    name = 'bench_compile',
    srcs = ['bench_compile.sh'],
    deps = ['//src:aptex'],
    testdata = [
        '//test:cnf',
        '//test:fmt',
    ],
)
