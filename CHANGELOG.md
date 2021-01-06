# lzlib

## 1.0.7.1

  * Bump C sources to lzlib 1.12

## 1.0.7.0

  * Add `Bounded` instance to `CompressionLevel`

## 1.0.6.0

  * Export `compressFineTune`

## 1.0.5.0

  * Export `LZError`

## 1.0.4.0

  * Add `Enum` instance to `CompressionLevel`

## 1.0.3.0

  * More laconic internals
  * Add `compressFileLevel` &c.

## 1.0.2.1

  * Tweak buffering

## 1.0.2.0

  * Improve haddocks

## 1.0.1.0

  * Export `lZApiVersion` and `lZVersion`

## 1.0.0.0

  * Remove `Codec.Lzip.Raw`
  * Support older GHCs
  * Use `ForeignPtr` for raw bindings
  * Stream lazily
  * Add `compressFile` and `compressWithSz`

## 0.3.3.0

  * Add `Exception` instance for `LZError`
  * Bug fixes

## 0.3.2.0

  * Add `compressBest` and `compressFast`

## 0.3.1.2

  * Hopefully improved performance

## 0.3.1.1

  * Bug fix + more sensible memory use

## 0.3.1.0

  * Export `lZApiVersion`

## 0.3.0.5

  * `bracket` things better

## 0.3.0.4

  * Use `bracket` and fix space leak

## 0.3.0.3

  * Use `bracket` so that memory is freed if an exception is thrown in another
    thread

## 0.3.0.2

  * Documentation improvements

## 0.3.0.1

  * Performance improvements

## 0.3.0.0

  * Take a `BSL.ByteString` as input
  * Performance improvements

## 0.2.0.2

  * Performance improvements

## 0.2.0.1

  * Performance improvements

## 0.2.0.0

  * Performance improvements
  * Pure functions no longer occur in IO
  * Change type signatures of compression/decompression functions

## 0.1.1.1

  * Fix distribution

## 0.1.1.0

  * Add a higher-level API
  * Add `Show` and `Eq` instances to `LZErrno` type
  * Bundle with `lzlib` version 1.11

## 0.1.0.0

Initial release
