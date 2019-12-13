# lzlib

[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/lzlib/badge)](https://matrix.hackage.haskell.org/package/lzlib)
[![Hackage](https://img.shields.io/hackage/v/lzlib.svg)](http://hackage.haskell.org/package/lzlib)

[![Dependencies of latest version on Hackage](https://img.shields.io/hackage-deps/v/lzlib.svg)](https://hackage.haskell.org/package/lzlib)

Haskell bindings to [lzlib](https://www.nongnu.org/lzip/lzlib.html).

This package includes a high-level interface to lzlib, however, it is slow.

## Hacking

Run

```bash
make -j
```

to download a few tarballs before running the test suite.

### CI

To edit the CI script, edit `haskell-ci.dhall` and regenerate
`.github/workflows/haskell.yml` with

```
make ci
```
