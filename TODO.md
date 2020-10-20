- [x] Fix space leak (?)
  - [x] Fix compression
- [x] Fix `hstar` streaming - couldn't pack `ghc-8.8.2` dir in constant space?
- [x] https://lz4.github.io/lz4/
- [ ] Pure Haskell version? data structures?
- [ ] Try `-fomit-frame-pointer`
# Bugs
- [ ] Hangs on `windows.img` (large files) for instance?
# Testing
- [ ] Test thread safety: partially decompress data, then finish decompression in another
  thread?
- [ ] valgrind
- [ ] cwe_checker (opam install cwe_checker)
- [ ] fbinfer
- [ ] https://github.com/TrustInSoft/tis-interpreter (ubsan...)
# Upstream
- [ ] Report mac performance issues upstream?
- [ ] Faster benchmarks w/ GHC 7.8.4 vs. GHC 8.10.1
  - [ ] Maybe bytestring?? -> reminds me of crc...
