This is the change file for CWEB's CWEAVE under Win32
(Contributed by Fabrice Popineau, February 2002)

@x section 1
@d banner "This is CWEAVE (Version 4.12.1)"
@y
@d banner "This is CWEAVE (Version 4.12.1win32)"
@z

@x section 32
bool names_match(
name_pointer p, /* points to the proposed match */
const char *first, /* position of first character of string */
size_t l, /* length of identifier */
eight_bits t) /* desired |ilk| */
@y
bool __cdecl names_match(
name_pointer p, /* points to the proposed match */
const char *first, /* position of first character of string */
size_t l, /* length of identifier */
eight_bits t) /* desired |ilk| */
@z
