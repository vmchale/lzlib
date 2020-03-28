- [x] Fix space leak (?)
  - [x] Fix compression
- [x] Fix `hstar` streaming - couldn't pack `ghc-8.8.2` dir in constant space?
- [x] https://lz4.github.io/lz4/
- [ ] Pure Haskell version? data structures?
# Testing
- [ ] Test thread safety: partially decompress data, then finish decompression in another
  thread?
# Upstream
- [ ] Report mac performance issues upstream?
