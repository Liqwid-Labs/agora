{
  description = "agora";

  nixConfig = {
    extra-experimental-features = [ "nix-command" "flakes" "ca-derivations" ];
    extra-substituters = [ "https://cache.iog.io" "https://mlabs.cachix.org" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
    max-jobs = "auto";
    auto-optimise-store = "true";
  };

  inputs = {
    nixpkgs.follows = "liqwid-nix/nixpkgs";
    nixpkgs-latest.url = "github:NixOS/nixpkgs";

    liqwid-nix = {
      url = "github:Liqwid-Labs/liqwid-nix/v2.3.0";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
    };

    liqwid-libs.url =
      "github:Liqwid-Labs/liqwid-libs?rev=050b2b6a3ee29dbba5bd43c38786b2947f45b2cb";
  };

  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.liqwid-nix.flakeModule
      ];
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          pkgs = import inputs.nixpkgs-latest { inherit system; };
        in
        {
          onchain.default = {
            src = ./.;
            ghc.version = "ghc925";
            fourmolu.package = pkgs.haskell.packages.ghc943.fourmolu_0_10_1_0;
            hlint = { };
            cabalFmt = { };
            hasktags = { };
            applyRefact = { };
            shell = { };
            enableBuildChecks = true;
            extraHackageDeps = [
              "${inputs.liqwid-libs}/plutarch-quickcheck"
              "${inputs.liqwid-libs}/plutarch-context-builder"
              "${inputs.liqwid-libs}/liqwid-plutarch-extra"
              "${inputs.liqwid-libs}/liqwid-script-export"
              "${inputs.liqwid-libs.inputs.ply}/ply-core"
              "${inputs.liqwid-libs.inputs.ply}/ply-plutarch"
            ];
          };
          ci.required = [ "all_onchain" ];
          packages.export =
            pkgs.stdenv.mkDerivation {
              name = "export";
              src = ./.;
              buildInput = [
                self'.packages."agora:exe:agora-scripts"
              ];
              buildPhase = ''
                export PATH=$PATH:${self'.packages."agora:exe:agora-scripts"}/bin
                agora-scripts file --builder raw
                agora-scripts file --builder rawDebug
              '';
              installPhase = ''
                NAME=${if self ? rev then self.shortRev else "dirty"}
                mkdir $out
                cp raw.json $out/agora-"$NAME".json
                cp rawDebug.json $out/agora-debug-"$NAME".json
              '';
            };
        };

      flake.hydraJobs.x86_64-linux = (
        self.checks.x86_64-linux
        // self.packages.x86_64-linux
      );
    };
}
