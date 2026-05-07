# Unit tests using Unity framework

cc_library(
    name = 'unity',
    srcs = ['vendor/unity/unity.c'],
    hdrs = [
        'vendor/unity/unity.h',
        'vendor/unity/unity_internals.h',
    ],
    incs = ['vendor/unity'],
    defs = ['UNITY_INCLUDE_DOUBLE'],
    warning = 'no',
)

cc_test(
    name = 'test_badness',
    srcs = ['test_badness.c'],
    deps = [':unity'],
    incs = ['vendor/unity'],
    defs = ['UNITY_INCLUDE_DOUBLE'],
    testdata = [],
)

cc_test(
    name = 'test_memory_word',
    srcs = ['test_memory_word.c'],
    deps = [':unity'],
    incs = ['vendor/unity'],
    defs = ['UNITY_INCLUDE_DOUBLE'],
    testdata = [],
)

cc_test(
    name = 'test_encoding',
    srcs = ['test_encoding.c'],
    deps = [
        ':unity',
        '//src:ptexenc',
        '//src:kpathsea',
    ],
    incs = [
        'vendor/unity',
        '../../texlive/texk/ptexenc',
        '../../texlive/texk/kpathsea',
        '../../texlive/texk',
    ],
    defs = ['UNITY_INCLUDE_DOUBLE'],
    testdata = [],
)

cc_test(
    name = 'test_scaled_arith',
    srcs = ['test_scaled_arith.c'],
    deps = [':unity'],
    incs = ['vendor/unity'],
    defs = ['UNITY_INCLUDE_DOUBLE'],
    testdata = [],
)

cc_test(
    name = 'test_node_alloc',
    srcs = ['test_node_alloc.c'],
    deps = [':unity'],
    incs = ['vendor/unity'],
    defs = ['UNITY_INCLUDE_DOUBLE'],
    testdata = [],
)
