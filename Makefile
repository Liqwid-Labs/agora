# This really ought to be `/usr/bin/env bash`, but nix flakes don't like that.
SHELL := /bin/sh

.PHONY: hoogle format haddock usage tag format_nix format_haskell format_check lint ps_bridge bench bench_check scripts

AGORA_TARGETS := agora agora-bench agora-purescript-bridge agora-scripts agora-specs agora-test agora-testlib

usage:
	@echo "usage: make <command> [OPTIONS]"
	@echo
	@echo "Available commands:"
	@echo "  hoogle -- Start local hoogle"
	@echo "  format -- Format the project"
	@echo "  haddock -- Generate Haddock docs for project"
	@echo "  tag -- Generate CTAGS and ETAGS files for project"
	@echo "  format_haskell -- Format haskell stuff, including source code and cabal files"
	@echo "  format_nix -- Format *.nix files only"
	@echo "  format_check -- Check if all haskell stuff have been formatted correctly"
	@echo "  lint -- Get hlint suggestions for project"
	@echo "  ps_bridge -- Generate purescript bridge files"
	@echo "  bench -- Generate bench report bench.csv"
	@echo "  bench_check -- Check if bench report is up-to-date"
	@echo "  scripts -- Export scripts to json files"

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
	hasktags -x $(AGORA_TARGETS)

lint:
	hlint $(AGORA_TARGETS)

PS_BRIDGE_OUTPUT_DIR := agora-purescript-bridge/
ps_bridge:
	cabal run exe:agora-purescript-bridge -- -o $(PS_BRIDGE_OUTPUT_DIR)

bench:
	cabal run agora-bench

BENCH_TMPDIR := $(shell mktemp -d)
BENCH_TMPFILE := $(BENCH_TMPDIR)/bench.csv
bench_check:
	(cabal run agora-bench -- -o "$(BENCH_TMPFILE)" \
		|| $(bench) -o "$(BENCH_TMPFILE)") >> /dev/null
	diff bench.csv $(BENCH_TMPFILE) \
		|| (echo "bench.csv is outdated"; exit 1)
	# TODO: do the clean-up even if `diff` fails.
	rm -rf $(BENCH_TMPDIR)

scripts:
	cabal run agora-scripts