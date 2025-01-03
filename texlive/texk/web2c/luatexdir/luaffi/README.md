About
-----
This is a library for calling C function and manipulating C types from [Ravi](https://github.com/dibyendumajumdar/ravi). It
is designed to be interface compatible with the FFI library in [LuaJIT's ffi](http://luajit.org/ext_ffi.html). 
It can parse C function declarations and struct definitions that have been directly copied out of C header files and
into lua source as a string.

This project is a fork of https://github.com/facebook/luaffifb which is a fork of https://github.com/jmckaskill/luaffi.

Source
------
https://github.com/dibyendumajumdar/ravi-ffi

Platforms
---------
Currently being developed for:
- Linux x64 - builds and tests pass
- OS X x64 - builds and tests pass
- Windows 10 x64 - builds and tests pass

Ravi and Lua 5.3 are supported. 

Build
-----
This project requires:

* CMake installation
* Ravi 5.3 installation

Windows 10
----------
* Note that only Visual Studio 2017 is supported. 
* Note that only x86-64 is supported

In the instructions below, we assume that Ravi was installed under `c:/Software/ravi`.

```bash
git clone https://github.com/dibyendumajumdar/ravi-ffi
cd ravi-ffi
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/Software/Ravi -G "Visual Studio 15 2017 Win64" ..
```

Above creates Visual Studio projects which can be used to build and install. Note that the install prefix ensures that the DLL files will be installed under /Software/ravi/bin so that they can be found by the library.

To build for Lua, change above to:
```
cmake -DCMAKE_INSTALL_PREFIX=/Software/lua53 -DUSE_LUA53=ON -G "Visual Studio 15 2017 Win64" ..
```
This assumes Lua 5.3 installation under `/Software/lua53'.

Documentation
-------------
This library is designed to be source compatible with LuaJIT's FFI extension. The documentation at http://luajit.org/ext_ffi.html describes the API and semantics.

Pointer Comparison
------------
Use `ffi.NULL` instead of `nil` when checking for `NULL` pointers.
```lua
  ffi.new('void *', 0) == ffi.NULL -- true
```

Known Issues
------------
- Comparing a ctype pointer to `nil` doesn't work the same as in LuaJIT (see above).
  This is unfixable with the current metamethod semantics.
- Constant expressions can't handle non integer intermediate values (eg
  offsetof won't work because it manipulates pointers)
- Not all metamethods work with Lua 5.1 (eg char* + number). This is due to
  the way metamethods are looked up with mixed types in Lua 5.1. If you need
this upgrade to Lua 5.2 or use boxed numbers (uint64_t and uintptr_t).
- All bitfields are treated as unsigned (does anyone even use signed
  bitfields?). Note that "int s:8" is unsigned on unix x86/x64, but signed on
windows.


How it works
------------
Types are represented by a struct ctype structure and an associated user value
table. The table is shared between all related types for structs, unions, and
functions. It's members have the types of struct members, function argument
types, etc. The struct ctype structure then contains the modifications from
the base type (eg number of pointers, array size, etc).

Types are pushed into lua as a userdata containing the struct ctype with a
user value (or fenv in 5.1) set to the shared type table.

Boxed cdata types are pushed into lua as a userdata containing the struct
cdata structure (which contains the struct ctype of the data as its header)
followed by the boxed data.

The functions in `ffi.C` provide the `cdata` and `ctype` metatables and ffi.*
functions which manipulate these two types.

C functions (and function pointers) are pushed into lua as a lua c function
with the function pointer cdata as the first upvalue. The actual code is JITed
using dynasm (see call_x86.dasc). The JITed code does the following in order:

1. Calls the needed unpack functions in `ffi.C` placing each argument on the HW stack
2. Updates `errno`
3. Performs the C call
4. Retrieves `errno`
5. Pushes the result back into lua from the HW register or stack
