#!/bin/bash

# Extensions necessary to tell fourmolu about 
EXTENSIONS="-o -XTypeApplications -o -XTemplateHaskell -o -XImportQualifiedPost -o -XPatternSynonyms -o -fplugin=RecordDotPreprocessor"
SOURCES=$(git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.hs')
nix run nixpkgs#haskellPackages.fourmolu -- --mode check --check-idempotence $EXTENSIONS $SOURCES
