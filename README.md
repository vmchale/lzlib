# lzlib

[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/lzlib/badge)](https://matrix.hackage.haskell.org/package/lzlib)
[![Hackage](https://img.shields.io/hackage/v/lzlib.svg)](http://hackage.haskell.org/package/lzlib)

[![Dependencies of latest version on Hackage](https://img.shields.io/hackage-deps/v/lzlib.svg)](https://hackage.haskell.org/package/lzlib)

Haskell bindings to [lzlib](https://www.nongnu.org/lzip/lzlib.html).

This package includes a high-level interface to lzlib. It provides performance
comparable to the [lzip](https://www.nongnu.org/lzip/) command-line tool.

## Comparison

Compared to the [lzip](http://hackage.haskell.org/package/lzip) Haskell library:

  * Uses `c2hs` instead of `hsc2hs`
  * Provides a high-level (`ByteString`-based) API

### Performance

Performance should be comparable to the
[lzip](http://download.savannah.gnu.org/releases/lzip/) C++ program.

## Hacking

Run

```bash
make
```

to download a few tarballs before running the test suite.
