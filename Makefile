# This really ought to be `/usr/bin/env bash`, but nix flakes don't like that.
SHELL := /bin/sh

.PHONY: hoogle format haddock usage tag format_nix format_haskell format_check	\
				lint refactor ps_bridge bench bench_check scripts test build ci

SOURCE_FILES := $(shell git ls-tree -r HEAD --full-tree --name-only)
SOURCE_FILES := $(wildcard $(SOURCE_FILES))
HASKELL_SOURCES := $(filter %.hs,$(SOURCE_FILES))
CABAL_SOURCES := $(filter %.cabal,$(SOURCE_FILES))
NIX_SOURCES := $(filter %.nix,$(SOURCE_FILES))
FORMAT_EXTENSIONS := -o -XQuasiQuotes -o -XTemplateHaskell -o -XTypeApplications	\
										-o -XImportQualifiedPost -o -XPatternSynonyms
HLINT_EXTS := -XQuasiQuotes

THREADS ?= 8
PS_BRIDGE_OUTPUT_DIR ?= agora-purescript-bridge/
BENCH_OUTPUT ?= bench.csv
TEST_CASE_TIMEOUT ?= 100

usage:
	@echo "usage: [env [<variable>=<value> ...]] make <command> [OPTIONS]"
	@echo
	@echo "Available variables:"
	@echo "  THREADS -- The number of threads for building the project"
	@echo "  PS_BRIDGE_OUTPUT_DIR -- The output directory of the purescript bridge"
	@echo "  BENCH_OUTPUT -- The output file of the benchmark report"
	@echo "  TEST_CASE_TIMEOUT -- Timeout for individual tests. Default unit: s"
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
	@echo "  scripts -- Run the agora script server (dev mode)"
	@echo "  ci -- Run all the CI checks"

requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"
	@ [ "$(IN_NIX_SHELL)" ] || (echo "    run 'nix develop' first" && false)

hoogle: requires_nix_shell
	pkill hoogle || true
	hoogle generate --local=haddock --database=hoo/local.hoo
	hoogle server --local -p 8081 >> /dev/null &
	hoogle server --local --database=hoo/local.hoo -p 8082 >> /dev/null &

format: format_haskell format_nix

format_nix: requires_nix_shell
	nixpkgs-fmt $(NIX_SOURCES)

format_haskell: requires_nix_shell
	fourmolu $(FORMAT_EXTENSIONS) -m inplace $(HASKELL_SOURCES)
	cabal-fmt -i $(CABAL_SOURCES)

format_check: requires_nix_shell
	fourmolu $(FORMAT_EXTENSIONS) -m check $(HASKELL_SOURCES)
	nixpkgs-fmt --check $(NIX_SOURCES) 
	cabal-fmt --check $(CABAL_SOURCES)

haddock: requires_nix_shell
	cabal haddock --haddock-html --haddock-hoogle --builddir=haddock

tag: requires_nix_shell
	hasktags -x $(HASKELL_SOURCES)

lint: requires_nix_shell
	hlint $(HLINT_EXTS) $(HASKELL_SOURCES)

refactor: requires_nix_shell
	for src in $(HASKELL_SOURCES) ; do \
		hlint $(HLINT_EXTS) --refactor --refactor-options='-i -s' $$src ;\
	done

ps_bridge: requires_nix_shell
	cabal run exe:agora-purescript-bridge -- -o $(PS_BRIDGE_OUTPUT_DIR)

bench: requires_nix_shell
	cabal run agora-bench -- -o $(BENCH_OUTPUT)

bench_check: requires_nix_shell
	cabal -v0 new-run agora-bench | diff bench.csv -

scripts: requires_nix_shell
	cabal run agora-scripts -- -c

test: requires_nix_shell
	cabal test --test-options="--hide-successes -t $(TEST_CASE_TIMEOUT) -j$(THREADS)"

build: requires_nix_shell
	cabal build -j$(THREADS)

ci:
	@ [[ "$$(uname -sm)" == "Linux x86_64" ]] || (echo "NOTE: CI only builds on Linux x86_64. Your system is $$(uname -sm), continuing...")
	nix build .#check.$(shell nix eval -f '<nixpkgs>' system)
