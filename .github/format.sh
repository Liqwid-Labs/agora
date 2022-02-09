#!/bin/bash

nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/refs/tags/21.05.tar.gz \
          -p haskellPackages.fourmolu \
          --run 'make format_check'
