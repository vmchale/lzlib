.PHONY: clean setup ci

setup: gmp-6.1.2.tar lzlib-1.10.tar

clean:
	rm -rf .stack-work dist-newstyle dist *.tar* *.hp *.prof stack.yaml.lock .hspec-failures

%.tar: %.tar.lz
	lzip --keep --decompress --force $@.lz

gmp-6.1.2.tar.lz:
	wget https://gmplib.org/download/gmp/gmp-6.1.2.tar.lz -O $@

lzlib-1.10.tar.lz:
	wget http://download.savannah.gnu.org/releases/lzip/lzlib/lzlib-1.10.tar.lz -O $@

ci: .github/workflows/haskell.yml

.github/workflows/haskell.yml: haskell-ci.dhall
	dhall-to-yaml --file $< --output $@
