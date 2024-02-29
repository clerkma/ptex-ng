
``pplib`` 2.x
=============

``pplib`` is a library for raw PDF access. It parses PDF documents and provides an interface to access document structures in C.

C-API
=====

Types
-----

``pplib`` defines several C-types to represent PDF types:

- ``ppbyte`` - char
- ``ppint`` - signed integer (``int64_t``)
- ``ppnum`` - real number (double)
- ``ppname`` - PDF name
- ``ppstring`` - PDF string
- ``pparray`` - PDF array
- ``ppdict`` - PDF dict
- ``ppstream`` - PDF stream
- ``ppref`` - PDF indirect reference
- ``ppobj`` - a container of all above

Among ``ppint`` and ``ppnum``, we also use ``ppuint`` - unsigned integer (machine word, alias to ``size_t``).

Other API types:

- ``ppdoc`` - PDF document
- ``ppxref`` - cross-references table
- ``ppcontext`` - ... later
- ``pprect`` - rectangle
- ``ppmatrix`` - matrix

Integer and number are as simple types. Names and strings used to be aliases to ``char *``
in ``pplib 1.x``, now they are structures; all related API functions operate on pointers.
Other types (array, dict, stream, reference, object container, xref, doc) are C-structures,
and you operate it their pointers. So when you declare a simple type variable you say::

  ppuint u;
  ppnum n;
  ...

And when you declare a compound type you operate on pointers::

  ppobj *obj;
  ppname *name;
  ppstring *string;
  pparray *array;
  ppdict *dict;
  ppstream *stream;
  ppref *ref;
  ppdoc *pdf;

Most of those C-types are defined in library header ``ppapi.h`` (complete types). Some others are incomplete
(eg. you can't say ``sizeof(ppdoc)`` or ``sizeof(ppxref)``). This is to avoid unnecesary dependencies in
the header. The library itself uses ``pplib.h`` header but for auxilary applications including a standalone
``ppapi.h`` header should be enough.

``pplib`` was designed having **read-only** PDF access in mind. Even if some structure is completelly exposed,
so that you can directly access its members, you should treat them as read-only. I don't make them ``const`` because
then all variable declarations would need to be ``const``, which is annoying, and I'd need some trickery in the library
internals to unconst. Besides, nothing is really const for C type casts.

Object
------

A common container for all elementary PDF object types is ``ppobj`` structure. ``ppobj`` has a type identifier
(integer) and union of values::

  struct ppobj {
    ppobjtp type;
    union {
      ppint integer;
      ppnum number;
      ppname *name;
      ppstring *string;
      pparray *array;
      ppdict *dict;
      ppstream *stream;
      ppref *ref;
      void *any;
    };
  };

Object type is one of constants (enum)::

  PPNONE
  PPNULL
  PPBOOL
  PPINT
  PPNUM
  PPNAME
  PPSTRING
  PPARRAY
  PPDICT
  PPSTREAM
  PPREF

The type determines the structure member you're allowed to access::

  ppobj *obj;
  ...
  switch (obj->type)
  {
    case PPNONE: // shouldn't actually happen, indicates some failure
      break;
    case PPNULL: // valid PDF null object, no value
      break;
    case PPBOOL: // do something with obj->integer (ppint), value 0 or 1
      break;
    case PPINT:  // do something with obj->integer (ppint)
      break;
    case PPNUM:  // do something with obj->number (ppnum)
      break;
    case PPNAME: // do something with obj->name (ppname *)
      break;
    case PPSTRING: // do something with obj->string (ppstring *)
      break;
    case PPARRAY: // do something with obj->array (pparray *)
      break;
    case PPDICT: // do something with obj->dict (ppdict *)
      break;
    case PPSTREAM: // do something with obj->stream (ppstream *)
      break;
    case PPREF: // do something with obj->ref (ppref *)
      break;
  }

More often then not you know exactly what type of object value is expected, in which case
you may use one of the following macros::

  // returns 1 if o->type is PPNULL
  int ppobj_get_null(o)         \

  // if o->type is PPBOOL, sets int v to 0 or 1 and returns 1, 0 otherwise
  int ppobj_get_bool(o, v)

  // if o->type is PPINT, sets ppint v and returns 1, 0 otherwise
  int ppobj_get_int(o, v)

  // if o->type is PPINT and >= 0, sets ppuint v and returns 1, 0 otherwise
  int ppobj_get_uint(o, v)

  // if o->type is PPNUM or PPINT, sets ppnum v and returns 1, 0 otherwise
  int ppobj_get_num(o, v)

  // if o->type is PPNAME returns the name, NULL otherwise
  ppname * ppobj_get_name(o)

  // if o->type is PPSTRING returns the string, NULL otherwise
  ppstring * ppobj_get_string(o)

  // if o->type is PPARRAY returns the array, NULL otherwise
  pparray * ppobj_get_array(o)

  // if o->type is PPDICT returns the dict, NULL otherwise
  ppdict * ppobj_get_dict(o)

  // if o->type is PPSTREAM returns the stream, NULL otherwise
  ppstream * ppobj_get_stream(o)

  // if o->type is PPREF returns the reference, NULL otherwise
  ppref * ppobj_get_ref(o)

Note the coercion from integer to real number, but not reverse. In practise, whenever you expect a real number,
you should also handle integer (eg. '1' used instead of '1.0' is pretty common in PDF).

It is a common case that the object is given as an indirect reference, but what you actually
want is not the reference, but the object referred by it. Here is a helper for it::

  // if o->type is PPREF, returns what the reference points, otherwise returns o
  ppobj * ppobj_rget_obj(o)

Also every ``ppobj_get_*`` macro has ``ppobj_rget_*`` counterpart that makes a check
for the expected type, but if the object is PPREF, it jumps to the target object.
So for example ``ppobj_rget_dict(obj)`` will return dict if ``obj`` is of type PPDICT
or if it is of type PPREF and ``obj->ref`` hosts an object of type PPDICT.

Names
-----

PDF names are represented as ``ppname`` pointer (``ppname`` used to be an alias to ``char *``, now it is a structure).
To access the name data::

  ppbyte * ppname_data(name) // name->data, bytes array

For convenient use in C, names are ``'\0'`` terminated. But to get the length of name better always use
``ppname_size()`` macro. ``ppname`` object knows its size, don't use ``strlen()``::

  size_t ppname_size(ppname name); // name->size, the length of name in bytes

In current implementation names are not hashed anyhow, so name-to-name comparison is not smarter than ``memcmp()``.
Use macros::

  int ppname_is(ppname name, "literal");    // to compare ppname with C-literal string
  int ppname_eq(ppname name, ppname other); // to compare ppname with a different name

If you use ``pplib`` to parse contents streams, you may need to distinguish names from operators
(more precisely executable names). Names in PDF are preceeded by '/', executable names aren't. In both
cases PDF parser will produce ``ppname`` but the result can be distingushed with::

  int ppname_exec(ppname name); // macro, returns non-zero if the name is executable

Names are kept in their raw form, with possible PDF specific escapes (in text below we call it **encoded** form).
Leading '/' is omitted. One may need a decoded name, with no PDF escapes.
A pair of functions provides a simple interface to switch between those two forms::

  // these helpers rely on name->alterego member
  ppname * ppname_decoded (ppname *name); // returns decoded (unescaped) form of the name
  ppname * ppname_encoded (ppname *name); // returns encoded (escaped) form of the name
  ppbyte * ppname_decoded_data (ppname *name);
  ppbyte * ppname_encoded_data (ppname *name);

In pretty most cases a PDF name contains only letters (no special characters, no escapes), so decoded and encoded forms are identical.
In that case both functions simply return the argument. It is ok to call ``ppname_decoded()`` on already decoded form
and ``ppname_encoded()`` on already encoded form. Both forms are produced by PDF objects parser, so accessing ``ppname`` alter ego
in whatever direction needs no extra decoding or allocation costs (the cost is paid by parser).

String
------

PDF strings have the same internal construction as names, so most of names description above applies to strings as well.
``ppstring`` is a structure (used to be an alias to ``char *``). ``string->data`` is ``\0``-terminated c-array of ``ppbytes``.
To get the data::

  ppbyte * ppstring_data(ppstring *string); // string->data, macro

To get the size of the string::

  size_t ppstring_size(ppstring *string); // macro, returns the length of the string in bytes

Strings are provided in their raw form, preserving PDF specific escapes, but with no
``()`` or ``<>`` delims. To distinguish plain strings from hex strings::

  int ppstring_hex(ppstring string); // macro, returns non zero if hex string

Or if you prefer::

  switch (ppstring_type(string))
  {
    case PPSTRING_PLAIN: // literal string, surrounded by ``(`` and ``)`` in PDF
      break;
    case PPSTRING_BASE16: // hex string, surrounded by ``<`` and ``>`` in PDF
      break;
    case PPSTRING_BASE85: // base85 string surrounded by ``<~`` and ``~>`` in PDF
      break;
  }

The last is actually Postscript specific, not used in PDF, but I think it might appear in contents streams...
No matter how the string is given in PDF (plain or hex), here are two functions to
switch between encoded and decoded strings forms::

  ppstring * ppstring_decoded (ppstring *string); // returns decoded string possibly with PDF escapes
  ppstring * ppstring_encoded (ppstring *string); // returns encoded string with no PDF escapes
  ppbyte * ppstring_decoded_data (ppstring *string);
  ppbyte * ppstring_encoded_data (ppstring *string);

For hex strings, encoded form contains hex digits, while decoded form contains arbitrary bytes (the result of hex decoding).
Plain strings usually contains printable ASCII characters, but they might contain any binary data.
As with names, objects parser produces both forms. The raw form with PDF escapes (or raw hex form) the main one.
Eg. when you access ``obj->string`` you always get the encoded form. At any moment you can switch to its alter ego.

No matter if the string is plain or hex, if its first two bytes (decoded) are UTF16 BOM, the string is unicode.
``ppstring`` object *knows* it is unicode or not::

  switch (ppstring_utf(string))
  {
    case PPSTRING_UTF16LE: // unicode string, utf16le
      break;
    case PPSTRING_UTF16BE: // unicode string, utf16be
      break;
    default:               // no unicode
  }

Or simply::

  if (ppstring_utf(string) != 0) {
    // handle unicode string
  }

If the string is unicode, BOM remains the part of the string  -- ``pplib`` parser does not strip it.
Unicode or not, encoded or decoded, strings are always C-arrays of bytes and ``ppstring_size()``
always returns the size in bytes.

Array
-----

PDF arrays are represented as ``pparray`` type, which is C-array of ``ppobj`` structures.
To get the size::

  size_t pparray_size(pparray *array) // macro, returns the number of array items

To get ``ppobj *`` at a given index::

  ppobj * pparray_at(array, index)  // macro, returns ppobj * (no index check)
  ppobj * pparray_get(array, index) // macro, returns ppobj * or NULL (with index check)
  ppobj * pparray_get_obj (pparray *array, size_t index);  // function equiv to pparray_get()

Iterating over array elements::

  pparray *array;
  size_t index, size;
  ppobj *obj;
  for (size = pparray_size(array), pparray_first(array, index, obj); index < size; pparray_next(index, obj))
  {
    // do something with index and obj
  }

There is no magic first/next macros, just iteration over pointers. One could also use something like::

  for (index = 0, size = array->size; index < size; ++index)
  {
    obj = pparray_at(array, index);
    // do something with index and obj
  }

When getting values from array and expecting a result of known type, use one of the following::

  int pparray_get_bool (pparray *array, size_t index, int *v);    // get boolean value
  int pparray_get_int (pparray *array, size_t index, ppint *v);   // get ppint value
  int pparray_get_uint (pparray *array, size_t index, ppuint *v); // get ppuint value
  int pparray_get_num (pparray *array, size_t index, ppnum *v);   // get ppnum value
  ppname * pparray_get_name (pparray *array, size_t index);       // get ppname * value
  ppstring * pparray_get_string (pparray *array, size_t index);   // get ppstring * value
  pparray * pparray_get_array (pparray *array, size_t index);     // get pparray * value
  ppdict * pparray_get_dict (pparray *array, size_t index);       // get ppdict * value
  ppref * pparray_get_ref (pparray *array, size_t index);         // get ppref * value

As with ``ppobj_get_*`` suite, numeric types getters set the value of a given type and returns 1, if the type matches.
Otherwise sets nothing and returns 0. Other getters return the value if the type matches, or NULL.

Every function from ``pparray_get_*`` suite have its ``pparray_rget_*`` counterpart
that dereferences indirect objects (as explained for ``ppobj_rget_*`` getters). Note that
there is no ``pparray_get_stream()`` function, as streams in PDF are always indirect
(may only reside in ``ref->object.stream``). To get the stream from array use::

  ppstream * pparray_rget_stream (pparray *array, size_t index);

Dict
----

PDF dicts are represented as ``ppdict`` structure, which is C-array of ``ppobj`` with parallel
C-array of ``ppname`` pointers. To get the size of a dict::

  size_t ppdict_size(ppdict *dict) // macro, returns the number of key-val pairs

To get the value at a given index (integer)::

  ppobj * ppdict_at(ppdict *dict, index) // macro, no index bounds check

To get the name (key) at a given index::

  ppname * ppdict_key(ppdict *dict, index) // macro, no index bounds check

To iterate over dict key-val pairs::

  ppdict *dict;
  ppname **pkey;
  ppobj *obj;

  for (ppdict_first(dict, pkey, obj); *pkey != NULL; ppdict_next(pkey, obj))
  {
    // do something with *pkey and obj
  }

There is no magic in first/next macros, just iteration through keys and values lists pointers.
For convenient iteration, a list of keys is terminated with NULL, so in the code above ``*pkey != NULL``
is used as the loop condition. One may also iterate via indices::

  ppdict *dict;
  size_t index, size;
  ppname *key;
  ppobj *obj;
  for (index = 0, size = ppdict_size(dict); index < size; ++index)
  {
    key = ppdict_key(dict, index);
    obj = ppdict_at(dict, index);
    // do something with key and obj
  }

To get the object associated with a given name, use one of the following::

  ppobj * ppdict_get_obj (ppdict *dict, const char *name);
  int ppdict_get_bool (ppdict *dict, const char *name, int *v);
  int ppdict_get_int (ppdict *dict, const char *name, ppint *v);
  int ppdict_get_uint (ppdict *dict, const char *name, ppuint *v);
  int ppdict_get_num (ppdict *dict, const char *name, ppnum *v);
  ppname ppdict_get_name (ppdict *dict, const char *name);
  ppstring ppdict_get_string (ppdict *dict, const char *name);
  pparray * ppdict_get_array (ppdict *dict, const char *name);
  ppdict * ppdict_get_dict (ppdict *dict, const char *name);
  ppref * ppdict_get_ref (ppdict *dict, const char *name);

Note that all getters accepts ``const char *`` as key, so it is ok to say::

  ppdict_rget_dict(dict, "Resources");

(the most common use I guess). But to use ``ppname`` object as a key, one have to
use data member::

  ppdic_rget_dict(dict, name->data); // ppname *name

Every ``ppdict_get_*`` getter has ``ppdict_rget_*`` counterpart that dereferences
indirect objects if necessary. Note that there is no ``ppdict_get_stream()`` function,
but there is::

  ppstream * ppdict_rget_stream (ppdict *dict, const char *name);

So far dicts comes with no names mapping, so by-name dict accessors perform a linear search
through the keys list. PDF dicts are usually small.

Stream
------

PDF streams are represented as ``ppstream`` objects. To get the stream dict::

  ppdict * ppstream_dict(ppstream *stream) // macro

To read the stream data::

  uint8_t * ppstream_first (ppstream *stream, size_t *size, int decode);
  uint8_t * ppstream_next (ppstream *stream, size_t *size);
  void ppstream_done (ppstream *stream);

Both ``first` and ``next`` functions return a chunk of stream data and sets the ``size`` of the chunk.
``decode`` parameter tell the reader to decompress the stream (1) or return raw (0). A call to ``ppstream_next()``
must be preceeded by ``ppstream_first()``. Once you're done with the stream, you have to call ``ppstream_done()``,
no matter if the stream has been read to the end or not. The stream data iterator in use::

  uint8_t *data;
  size_t size;
  ppstream *stream;
  int decode = 1; // 1 - get decompressed, 0 - get raw

  for (data = ppstream_first(stream, &size, decode); data != NULL; data = ppstream_next(stream, &size))
  {
    // do something with data and its size
  }
  ppstream_done(stream);

Every subsequent iterator call invalidates the previous reader output, so you have to utilize the returned chunk
of data just after you ot that. So the following is wrong::

  data1 = ppstream_first(stream, &size, 1);
  data2 = ppstream_next(stream, &size);
  data3 = ppstream_next(stream, &size);
  some_output(data1, size);
  some_output(data2, size);

The reader calls usually return the same pointer to internal buffer, just filled with a different data.
``pplib`` allocates reasonably large buffer and fills that buffer on subsequent calls to the reader.

If the source stream has no compression, using both ``decode == 1`` and ``decode == 0`` should give the same result.
You can check if the stream is actually compressed with::

  ppstream_compressed(stream) // macro, returns non zero if /Filter is present

It might be necessary to load the entire stream data at once::

  uint8_t * ppstream_all (ppstream *stream, size_t *size, int decode);

If the initial buffer size is insufficient, it grows until the entire stream data is loaded. You must call
``ppstream_done(stream)`` after using returned data.

``ppstream_done()`` doesn't invalidate the stream object, it just closes its internal reader.
The stream itself remains a valid object (eg. one can read it again if necessary),
but the reader buffer is released. It is actually not freed but kept for future the reuse with that on some other stream,
but you still need to mark it ready for reuse to avoid allocating a separate buffer for every stream you're going to read.

Stream data readers will return ``NULL`` if you haven't close the previous reader process  with ``ppstream_done()``. All below is wrong::

  data1 = ppstream_all(stream, &size, 1);
  data2 = ppstream_all(stream, &size, 1); // data2 == NULL
  // or
  data1 = ppstream_first(stream, &size, 1);
  data2 = ppstream_first(stream, &size, 1); // data2 == NULL
  // or
  data1 = ppstream_first(stream, &size, 1);
  data2 = ppstream_all(stream, &size, 1); // data2 == NULL

To avoid unnecessary dependencies, ``pplib`` does not support image filters (``/DCT``, ``/JPX``, ``/JBIG``, ``/CCITT``).
But it is ok to read the stream with ``decode`` set to 1 on such streams. ``pplib`` assumes that the image is the
final/target stream form and just returns it as-is. Eg. in the case of JPEG (``/DCT`` filtered) image both calls should
give the same results::

  ppstream_all(jpegstream, &jpegsize, 0); // don't decode, return what's there
  ppstream_all(jpegstream, &jpegsize, 1); // decode but found image filter, effectively the same

A bit more about streams memory. As mentioned, ``pplib`` allocates buffers for stream readers. After ``ppstream_done()``,
the stream no longer *owns* the buffer space. But the buffer may remain allocated, to be reused with future readers.
``pplib`` keeps a pool of several buffers. This means, that when you use stream readers, ``pplib`` eats
some memory (1MB or so) that is not freed, even if no streams are used. And even if you free all objects.
If you suffer from this, you can optionally use a pair of functions::

  void ppstream_init_buffers (void);
  void ppstream_free_buffers (void);

The first initializes buffers pool, unless done so far. Currently ``pplib`` cares of it before opening every stream reader,
so it is not obligatory. The second frees a pool of buffers. The intended use is to call ``ppstream_init_buffers()`` once
as kind of library initializer and to call ``ppstream_free_buffers()`` once, as the library finalizer.

Filters
-------

``ppstream`` knows its filter(s) and keps it as ``stream->filter``::

  // ppstream *stream;
  ppstream_filter *info = &stream->filter;

``ppstream_filter`` is the following structure::

  typedef struct {
    ppstreamtp *filters; // c-array of filter identifiers (enum integers)
    ppdict **params;     // c-array of ppdict pointers
    size_t count;        // number of filters, length of the arrays (typically 1)
  } ppstream_filter;

If ``count > 0`` then ``filters`` member is not NULL. Filters array keeps integer constants::

  PPSTREAM_BASE16    /* /ASCIIHexDecode  */
  PPSTREAM_BASE85    /* /ASCII85Decode   */
  PPSTREAM_RUNLENGTH /* /RunLengthDecode */
  PPSTREAM_FLATE     /* /FlateDecode     */
  PPSTREAM_LZW       /* /LZWDecode       */
  PPSTREAM_CCITT     /* /CCITTFaxDecode  */
  PPSTREAM_DCT       /* /DCTDecode       */
  PPSTREAM_JBIG2     /* /JBIG2Decode     */
  PPSTREAM_JPX       /* /JPXDecode       */
  PPSTREAM_CRYPT     /* /Crypt           */

Params array keeps corresponding filter parameters (``/DecodeParms``) if present. ``params`` member is not NULL
if ``count > 0`` and the stream dict has ``/DecodeParms`` entry. Even if ``params`` is there,
for every N-th filter, ``params[N]`` may be NULL (corresponding to PDF ``null``).

``stream->filter`` keeps the source stream filter information, which may not correspond to the result of stream readers
(``ppstream_first()``, ``ppstream_next()``, ``ppstream_all()``). The get the filters info relevant to the result from readers::

  void ppstream_filter_info (ppstream *stream, ppstream_filter *info, int decode);

The function fills ``ppstream_filter`` structure according to the expected result from stream readers (example 3 shows
how to use it to reconstruct ``/Filter`` and ``/DecodeParms`` when copying the stream to some other PDF).

To convert filter identifier (``ppstreamtp``) to a corresponding PDF filter name::

  const char * ppstream_filter_name[];

To covert ``ppname`` to filter identifier::

  int ppstream_filter_type (ppname filtername, ppstreamtp *filtertype);
  // returns 1 and sets filtertype if filtername is the proper filter name

Additional information about the stream can be fetched from macros::

  ppstream_compressed(stream) /* stream->flags & (PPSTREAM_FILTER|PPSTREAM_IMAGE) */
  ppstream_filtered(stream)   /* stream->flags & PPSTREAM_FILTER */
  ppstream_image(stream)      /* stream->flags * PPSTREAM_IMAGE */

``stream->flags`` is a binary sum of the following::

  PPSTREAM_FILTER        /* set iff the stream filters list has one of: BASE16, BASE85, RUNLENGTH, FLATE, LZW */
  PPSTREAM_IMAGE         /* set iff the stream filters list has one of: CCITT, DCT, JBIG2, JPX */
  PPSTREAM_ENCRYPTED     /* set iff the stream is encrypted */
  PPSTREAM_ENCRYPTED_OWN /* set iff the stream has own CRYPT filter */

Note that ``PPSTREAM_COMPRESSED`` is not there any longer, use ``ppstream_compressed()`` instead.
And there is some more, see ``ppapi.h``.

Ref
---

Indirect objects are represented as ``ppref`` structure. To get the object that the
reference refers to::

  ppobj * ppref_obj(ppref *ref) // macro

``ppref`` structure also keeps the reference number and version, a pointer to cross reference table it belongs
to and others, but I guess you won't need anything but the referenced object. ``pplib`` parser resolves references
on-fly. So if there is a dict with indirect objects::

  <<
    /Type /Page
    /Resources 123 0 R
    ...
  >>

the parser will produce ``ppdict`` with ``Resources`` key pointing the proper ``ppref *`` value.
If you need more, access ``ppref`` members::

  struct ppref {
    ppobj object;           // target object
    ppuint number, version; // identifiers
    size_t offset;          // file offset (useless for you, may be zero for compressed objects)
    ppuint length;          // the length of the original object data
    ppxref *xref;           // cross reference table it belongs to
  };


XRef
----

Cross reference table is exposed as ``ppxref`` (incomplete type, you can only oprate on its pointer).
To get top document xref::

  ppxref * ppdoc_xref (ppdoc *pdf);

To get previous (older) xref::

  ppxref * ppxref_prev (ppxref *xref);

To find an object of a given refnumber::

  ppref * ppxref_find (ppxref *xref, ppuint refnumber);

[Note: since pplib v0.98 in case of documents with incremental update, ``ppxref_find()`` returns
the newest available version of a given object rather than the object in a given body.]

PDF
---

PDF document is represented as ``ppdoc`` structure (incomplete type, you can only operate on its pointer).
To load a document from file::

  ppdoc * ppdoc_load (const char *filename);

To load a document from file handle::

  ppdoc * ppdoc_filehandle (FILE *file, int closefile); // closefile 1 to fclose() on end

To load a document from memory data::

  ppdoc * ppdoc_mem (const void *data, size_t size);

The data is assumed to be a buffer allocated with ``malloc`` - it is freed when destroying ``ppdoc``.

Both loaders returns ``NULL`` on failure.

To free ``ppdoc`` and all objects it refers to::

  void ppdoc_free (ppdoc *pdf);

So far we haven't mention about any explicit object reclaimers. There are no dedicated ``free`` functions
for other objects. You don't allocate or free objects yourself. ``ppdoc`` object is an owner of all
beings it refers to. It also means that every object described so far is alive as long as the containing
``ppdoc`` is alive.

To access main PDF dicts::

  ppdict * ppdoc_trailer(ppdoc *pdf); // returns top xref trailer dict
  ppdict * ppdoc_catalog(ppdoc *pdf); // returns catalog referred from the trailer
  ppdict * ppdoc_info(ppdoc *pdf);    // returns info dict referred from the trailer

To get the PDF version::

  const char * ppdoc_version_string (ppdoc *pdf);    // version string
  int ppdoc_version_number (ppdoc *pdf, int *minor); // minor and major numbers

To get the file size of the source PDF document::

  size_t ppdoc_file_size (ppdoc *pdf);

To get the number of objects in all xrefs::

  ppuint ppdoc_objects (ppdoc *pdf);

To get the approx usage of memory::

  size_t ppdoc_memory (ppdoc *pdf, size_t *waste);

Encryption
----------

``pplib`` handles encrypted (password protected) documents. If a document is encrypted, most of strings and streams are ciphered.
In that form they are unreadable and rather useless, you can't even rewrite such strings/streams as-is to a different PDF output.
It is a common practise to *protect* documents with an empty password. Such documents remain readable in Acrobat (just opens them without prompting
for a password), but some features (eg. printing) may restricted by the application.

When ``pplib`` detects encryption, it follows Acrobat approach and first tries an empty password. If it succeeds, ``pplib`` proceeeds normally, providing
an access to decrypted strings and streams, as if they weren't ciphered. If the document is protected with non-empty password, you have to call
``ppdoc_crypt_pass()``. Until you provide a password, ``ppdoc`` object returned by ``ppdoc_load()`` function has all object entries set to ``null``.

Once the document is loaded, encryption status can be checked with::

  ppcrypt_status ppdoc_crypt_status (ppdoc *pdf);

``ppcrypt_status`` (enum) may have the following values:

  ``PPCRYPT_NONE`` - no encryption, go ahead
  ``PPCRYPT_DONE`` - encryption present but password succeeded, go ahead
  ``PPCRYPT_PASS`` - encryption present, need non-empty password
  ``PPCRYPT_FAIL`` - invalid or unsupported encryption (eg. undocumented in pdf spec)

If a password is needed, you can provide one with::

  ppcrypt_status ppdoc_crypt_pass (ppdoc *pdf, const void *userpass, size_t userpasslength,
                                               const void *ownerpass, size_t ownerpasslength);

Providing one of two is enough to decrypt the document.
It is ok to use the same password for owner and user -- ``pplib`` will try both.

The function returns ``PPCRYPT_DONE`` if the password succeeds and the previous crypt status otherwise.

Your custom loader function may look like that::

  ppdoc *pdf;
  pdf = ppdoc_load("file.pdf");
  if (pdf == NULL)
    return NULL;
  switch (ppdoc_crypt_status(pdf))
  {
    case PPCRYPT_NONE:
    case PPCRYPT_DONE:
      return pdf;
    case PPCRYPT_PASS:
      if (ppdoc_crypt_pass(pdf, "dummy", 5, "dummy", 5) == PPCRYPT_DONE)
        return pdf;
      printf("sorry, password needed\n");
      ppdoc_free(pdf);
      return NULL;
    case PPCRYPT_FAIL:
      printf("sorry, encryption failed\n");
      ppdoc_free(pdf);
      return NULL;
  }

See ``pplib`` tests suite for a complete code.

If you'd like to know what permissions are given/restricted to encrypted document::

  ppint ppdoc_permissions (ppdoc *pdf);

Returned value can be queried with the following binary flags (you can verify with Acrobat *File -> Properties -> Security* tab)::

  PPDOC_ALLOW_PRINT       // printing
  PPDOC_ALLOW_MODIFY      // filling form fields, signing, creating template pages
  PPDOC_ALLOW_COPY        // copying, copying for accessibility
  PPDOC_ALLOW_ANNOTS      // filling form fields, copying, signing
  PPDOC_ALLOW_EXTRACT     // contents copying for accessibility
  PPDOC_ALLOW_ASSEMBLY    // (no effect)
  PPDOC_ALLOW_PRINT_HIRES // (no effect)

``pplib`` does absolutelly nothing with permissions, it cares only to decrypt the document. As mentioned, encryption applies to strings
and streams. ``pplib`` decrypt strings when parsing document objects, so the result you get is *normal* (not ciphered).
Streams are decrypted whenever you access them. Even if you ask for a raw stream data, you'll get a raw (compressed) stream, but decrypted.
So except the check to ``ppdoc_crypt_status()``, you shouldn't bother about encryption.

In encrypted documents most of streams are encrypted. To check if a given stream is encrypted::

  ppstream_encrypted(stream) // macro, returns non-zero if encrypted

Encryption is independent from compression, don't confuse with ``ppstream_compressed()``

.. caution::
   Starting from ``pplib v2.10`` all passwords should be passed as ``UTF-8``.
   Earlier PDF encryption algorithms (/V 1..4, PDF 1.7) were based on ``PdfDocEncoding``.
   Newer algorithms (/V 5, PDF 1.8-2.0) expect Unicode encoded as ``UTF-8``.
   ``pplib`` API now expects ``UTF-8`` and if opening documents with older encryption methods,
   it tries to make a conversion to 8-bit encoding. In earlier versions ``pplib`` didn't
   make any password preprocessing, treating them as raw byte arrays.
   So in case of passwords with fancy characters, it may behave differently.

Pages
-----

Several helpers to deal with pages. To get the number of pages::

  ppuint ppdoc_page_count (ppdoc *pdf);

To access the root pages tree node::

  ppref * ppdoc_pages(ppdoc *pdf);

To get the page reference at a given index::

  ppref * ppdoc_page (ppdoc *pdf, ppuint index);

``index`` is a page number. First page has number 1. For index out of bounds ``ppdoc_page()`` returns NULL.
Iterating over pages using index from 1 to ``ppdoc_page_count()`` and calling ``ppdoc_page()`` on each iteration
would be suboptimal. Here is a dedicted iterator for this::

  ppref *  ppdoc_first_page (ppdoc *pdf);
  ppref * ppdoc_next_page (ppdoc *pdf);

The iterator in use::

  ppdoc *pdf;
  ppref *ref;
  ppdict *dict;
  int pageno;

  pdf = ppdoc_load("file.pdf");
  for (ref = ppdoc_first_page(pdf), pageno = 1; ref != NULL; ref = ppdoc_next_page(pdf), ++pageno)
  {
    dict = ppref_obj(obj)->dict; // take for granted it is a dict
    // do something with the page dict
  }

Functions related to pages return ``ppref *`` ensured to contain dict object, so you don't need sanity
type checks here.

Contents
--------

PDF page contents can be given as a stream or array of streams. Here is a convenience iterator over page
contents streams::

  ppstream * ppcontents_first (ppdict *dict);
  ppstream * ppcontents_next (ppdict *dict, ppstream *stream);

A complete example of contents stream parser use is given below (example 2).
But before we get there, we need to introduce ``ppcontext`` object. Conceptually,
``ppcontext`` is an owner (memory handler) of objects created on demand (beyond the ``ppdoc``).
So far used only with contents stream parser, which might produce quite some data that we want
to release just after used. To create a new context::

  pcontext * ppcontext_new (void);

It initializes a new context and its internal memory heap, taking about 64kB on start. After that,
the context is ready to produce objects (contents parsing functions below). Once objects produced
from a given context are no longer needed::

  void ppcontext_done (ppcontext *context);

It restores the context to its initial state, as after ``ppcontext_new()``. It means that the context
is ready to produce another bunch of beings (in the example below, all objects from the next page contents).
Once the context is not needed anymore::

  void ppcontext_free (ppcontext *context);

Now, contents stream parser functions take the context as an argument. Iterator form of contents stream parser
that allows to process the contents operator by operator::

  ppobj * ppcontents_first_op (ppcontext *context, ppstream *stream, size_t *psize, ppname *pname);
  ppobj * ppcontents_next_op (ppcontext *context, ppstream *stream, size_t *psize, ppname *pname);

Returned ``ppobj *`` is a pointer to operands list. ``*psize`` is the number of operands on stack.
The operator itself is stored as ``*pname``.

To parse the entire contents stream at once with no stop at every operator::

  ppobj * ppcontents_parse (ppcontext *context, ppstream *stream, size_t *psize);

Returns probably quite long list of all parsed objects (operands and operatos) in one piece.
The number of objects is stored to ``*psize``.

[Contents may contain so called inline images, that breaks a simple scheme of operands / operator syntax::

  BI <keyval pairs> ID <binary image data> EI

Contents parser treats this genuine triplet as a single piece, producing two operands (dict and string)
followed by ``EI`` operator name.]

Boxes
-----

Boxes (rectangles) in PDF are roughly 4-number arrays, but with a special intent.
``pplib`` provides a basic interface for these special arrays::

  typedef struct {
    ppnum lx, ly, rx, ry;
  } pprect;

This type is used only by helper functions - PDF parser is not aware of the rectangle type.
To convert ``pparray`` to ``pprect``::

  pprect * pparray_to_rect (pparray *array, pprect *rect); // returns rect or NULL

In example::

  pprect rect;
  if (pparray_to_rect(array, &rect) != NULL)
   ; // do something with rect

To get some image bounding box::

  pprect * ppdict_get_rect (ppdict *dict, const char *name, pprect *rect);
  // eg. ppdict_get_rect(imagedict, "BBox", &rect)

To get some page box::

  pprect * ppdict_get_box (ppdict *dict, const char *name, pprect *rect);
  // eg. ppdict_get_box(pagedict, "MediaBox", &rect)

The later not only checks the pagedict, but also goes through parent page nodes.

Transforms
----------

Transformations are given as 6-number arrays, but with a special intent.
``pplib`` provides a basic interface for these special arrays::

  typedef struct {
    ppnum xx, xy, yx, yy, x, y;
  } ppmatrix;

This type is used only by helper functions - PDF parser is not aware of the matrix type.
To convert ``pparray`` to ``ppmatrix``::

  ppmatrix * pparray_to_matrix (pparray *array, ppmatrix *matrix);

In example::

  ppmatrix matrix;
  if (pparray_to_matrix(array, &matrix) != NULL)
    ; // do something with matrix

To get the matrix from dict::

  ppmatrix * ppdict_get_matrix (ppdict *dict, const char *name, ppmatrix *matrix);
  // eg. ppdict_get_matrix(imagedict, "Matrix", &matrix)

Errors handling
---------------

``pplib`` is not verbose, but might happen that it needs to log some error message, eg. when parsing
of some PDF boject fails due to invalid offsets. By default, ``pplib`` prints the message to stdout, eg.::

  invalid 123 0 R object at offset 123123

To replace the default logger, you can provide your own::

  void pplog_callback (pplogger_callback logger, void *alien);

``pplogger_callback`` is a function::

  void your_callback (const char *message, void *alien);

In example, to redirect messages to stderr you may define a function::

  void your_callback (const char *message, void *alien)
  {
    fprintf((FILE *)alien, "\nooops: %s\n", message);
  }

Then set the callback somewhere before loading documents::

  pplog_callback(your_callback, stderr);

(example 2 uses that).

To set the default log messages prefix, eg. ``pplib:``, use::

  int pplog_prefix (const char *prefix)

Default is empty. The function succeeds if provided prefix is reasonably short (less then 32 bytes).
