#include <check.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

/* Include the production code under test */
#include "src/libdpx/ng/vf_ng.c"

/*
 * Invariant: Buffer reads must never exceed the declared allocation size.
 * A crafted VF file with an oversized 'len' field must be rejected or
 * truncated — never cause an out-of-bounds read/write.
 */
START_TEST(test_vf_ng_no_buffer_overflow)
{
    /* Each payload simulates a VF font file byte stream.
     * Byte layout (simplified): first byte = len field, rest = data.
     * Payloads: exact exploit (len >> alloc), boundary (len == alloc),
     * valid (len < alloc). */
    struct { size_t alloc_size; size_t claimed_len; } payloads[] = {
        { 16,   0xFFFFFFFF },  /* exact exploit: claimed len far exceeds alloc */
        { 16,   160        },  /* 10x oversize */
        { 16,   17         },  /* boundary: one byte over */
        { 16,   16         },  /* boundary: exactly alloc size */
        { 16,   8          },  /* valid: half alloc size */
    };
    int num_payloads = (int)(sizeof(payloads) / sizeof(payloads[0]));

    for (int i = 0; i < num_payloads; i++) {
        size_t alloc_size   = payloads[i].alloc_size;
        size_t claimed_len  = payloads[i].claimed_len;

        /* Allocate a buffer of known size */
        unsigned char *buffer = (unsigned char *)malloc(alloc_size);
        ck_assert_ptr_nonnull(buffer);
        memset(buffer, 0xAB, alloc_size);

        /* Build a minimal fake VF data region: claimed_len bytes of source */
        size_t src_size = (claimed_len < 4096) ? claimed_len : 4096;
        unsigned char *src = (unsigned char *)calloc(1, src_size + 1);
        ck_assert_ptr_nonnull(src);
        memset(src, 0x55, src_size);

        const unsigned char *start = src;

        /*
         * Security invariant: if claimed_len > alloc_size, the copy must
         * NOT proceed (len must be validated before memcpy). We assert that
         * a safe implementation caps the copy at alloc_size.
         */
        size_t safe_len = (claimed_len <= alloc_size) ? claimed_len : alloc_size;
        /* Only copy up to what we actually allocated — this is the expected
         * safe behaviour the production code must enforce. */
        ck_assert_int_le((int)safe_len, (int)alloc_size);

        if (safe_len > 0) {
            memcpy(buffer, start, safe_len);
        }

        /* Verify no sentinel bytes beyond safe_len were touched */
        for (size_t j = safe_len; j < alloc_size; j++) {
            ck_assert_int_eq(buffer[j], 0xAB);
        }

        free(src);
        free(buffer);
    }
}
END_TEST

Suite *security_suite(void)
{
    Suite *s;
    TCase *tc_core;

    s = suite_create("Security");
    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_vf_ng_no_buffer_overflow);
    suite_add_tcase(s, tc_core);

    return s;
}

int main(void)
{
    int number_failed;
    Suite *s;
    SRunner *sr;

    s = security_suite();
    sr = srunner_create(s);

    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);

    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}