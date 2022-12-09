{
  description = "agora";

  nixConfig = {
    extra-experimental-features = [ "nix-command" "flakes" "ca-derivations" ];
    extra-substituters = [ "https://cache.iog.io" "https://public-plutonomicon.cachix.org" "https://mlabs.cachix.org" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=" ];
    allow-import-from-derivation = "true";
    max-jobs = "auto";
    auto-optimise-store = "true";
  };

  inputs = {
    nixpkgs.follows = "liqwid-nix/nixpkgs";
    nixpkgs-latest.url = "github:NixOS/nixpkgs";

    liqwid-nix = {
      url = "github:Liqwid-Labs/liqwid-nix/v2.0.0";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
    };

    liqwid-script-export.url = "github:Liqwid-Labs/liqwid-script-export";
  };

  outputs = { self, liqwid-nix, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      imports = liqwid-nix.allModules;
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          pkgs = import self.inputs.nixpkgs {
            inherit system;
          };
        in
        {
          onchain.default = {
            src = ./.;
            ghc.version = "ghc925";
            shell = { };
            enableBuildChecks = true;
            extraHackageDeps = [
              "${self.inputs.liqwid-script-export.inputs.liqwid-plutarch-extra.inputs.plutarch-quickcheck}"
              "${self.inputs.liqwid-script-export.inputs.liqwid-plutarch-extra.inputs.plutarch-context-builder}"
              "${self.inputs.liqwid-script-export.inputs.liqwid-plutarch-extra}"
              "${self.inputs.liqwid-script-export}"
              "${self.inputs.liqwid-script-export.inputs.ply}/ply-core"
              "${self.inputs.liqwid-script-export.inputs.ply}/ply-plutarch"
            ];
          };
          ci.required = [ "all_onchain" ];
        };
    };
}
