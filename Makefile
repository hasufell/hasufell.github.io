rebuild:
	@rm -rf docs
	@cabal run site -- clean
	@cabal run site -- build
	@cp -a _site/ docs


.PHONY: rebuild
