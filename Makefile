.PHONY: clean setup

MAKEFLAGS += --warn-undefined-variables --no-builtin-rules -j
.DELETE_ON_ERROR:

setup: gmp-6.1.2.tar lzlib-1.10.tar

clean:
	rm -rf .stack-work dist-newstyle dist *.tar* *.hp *.prof stack.yaml.lock .hspec-failures tags *.svg

%.tar: %.tar.lz
	lzip --keep --decompress --force $@.lz

gmp-6.1.2.tar.lz:
	curl https://gmplib.org/download/gmp/gmp-6.1.2.tar.lz -o $@

lzlib-1.10.tar.lz:
	curl http://download.savannah.gnu.org/releases/lzip/lzlib/lzlib-1.10.tar.lz -o $@ -L
