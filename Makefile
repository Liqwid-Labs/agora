SHELL := /usr/bin/env bash

.PHONY: hoogle format usage

usage:
	@echo "usage: make <command> [OPTIONS]"
	@echo
	@echo "Available commands:"
	@echo "  hoogle -- Start local hoogle"
	@echo "  format -- Format the project"

HOOGLE_PORT=8081
hoogle:
	hoogle server --local --port $(HOOGLE_PORT)

FORMAT_EXTENSIONS := -o -XQuasiQuotes -o -XTemplateHaskell -o -XTypeApplications -o -XImportQualifiedPost -o -XPatternSynonyms -o -fplugin=RecordDotPreprocessor
format:
	find -name '*.hs' -not -path './dist-*/*' | xargs fourmolu $(FORMAT_EXTENSIONS) -m inplace
	git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.nix' | xargs nixfmt
	git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.cabal' | xargs cabal-fmt -i
