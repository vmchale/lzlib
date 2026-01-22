# lzlib

Haskell bindings to [lzlib](https://www.nongnu.org/lzip/lzlib.html).

This package includes a high-level interface to lzlib based on `ByteString`s.

## BUGS

The test suite segfaults when `-with-rtsopts=-N` is supplied.
