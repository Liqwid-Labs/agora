# This really ought to be `/usr/bin/env bash`, but nix flakes don't like that.
SHELL := /bin/sh

.PHONY: hoogle format haddock usage tag lint ps_bridge

usage:
	@echo "usage: make <command> [OPTIONS]"
	@echo
	@echo "Available commands:"
	@echo "  hoogle -- Start local hoogle"
	@echo "  format -- Format the project"
	@echo "  haddock -- Generate Haddock docs for project"
	@echo "  tag -- Generate CTAGS and ETAGS files for project"
	@echo "  lint -- Get hlint suggestions for project"
	@echo "  ps_bridge -- Generate purescript bridge files"

hoogle:
	pkill hoogle || true
	hoogle generate --local=haddock --database=hoo/local.hoo
	hoogle server --local -p 8081 >> /dev/null &
	hoogle server --local --database=hoo/local.hoo -p 8082 >> /dev/null &

format: format_haskell format_nix

format_nix:
	git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.nix' | xargs nixpkgs-fmt

FORMAT_EXTENSIONS := -o -XQuasiQuotes -o -XTemplateHaskell -o -XTypeApplications -o -XImportQualifiedPost -o -XPatternSynonyms -o -XOverloadedRecordDot
format_haskell:
	find -name '*.hs' -not -path './dist-*/*' | xargs fourmolu $(FORMAT_EXTENSIONS) -m inplace
	git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.cabal' | xargs cabal-fmt -i

format_check:
	find -name '*.hs' \
	     -not -path './dist*/*' \
	     -not -path './haddock/*' \
	  | xargs fourmolu $(FORMAT_EXTENSIONS) -m check

haddock:
	cabal haddock --haddock-html --haddock-hoogle --builddir=haddock

tag:
	hasktags -x agora agora-bench agora-test agora-testlib agora-sample agora-purescript-bridge

lint:
	hlint agora agora-bench agora-test agora-testlib agora-sample agora-purescript-bridge

PS_BRIDGE_OUTPUT_DIR := agora-purescript-bridge/
ps_bridge:
	cabal run exe:agora-purescript-bridge -- -o $(PS_BRIDGE_OUTPUT_DIR)
