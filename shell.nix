# This file is no longer useful for normal nix-shell, as it has
# been superseded by flake.nix However, this is still useful for
# lorri to use automatically.
{ system ? builtins.currentSystem }:

(builtins.getFlake (toString ./.)).devShell.${system}
