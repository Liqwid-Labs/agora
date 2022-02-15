#!/bin/bash

# Extensions necessary to tell fourmolu about
EXTENSIONS="-o -XTypeApplications -o -XTemplateHaskell -o -XImportQualifiedPost -o -XPatternSynonyms -o -XOverloadedRecordDot"
SOURCES=$(git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.hs')
nix run nixpkgs#haskell.packages.ghc921.fourmolu -- --mode check --check-idempotence $EXTENSIONS $SOURCES
