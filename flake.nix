{
  description = "agora";

  inputs.nixpkgs.follows = "plutarch/nixpkgs";
  inputs.haskell-nix.follows = "plutarch/haskell-nix";

  # https://github.com/mlabs-haskell/apropos-tx/pull/28
  inputs.apropos-tx.url =
    "github:mlabs-haskell/apropos-tx?rev=5b74ba897a6f02718c163bf588a08c5e3e9de204";
  inputs.apropos-tx.inputs.nixpkgs.follows =
    "plutarch/haskell-nix/nixpkgs-unstable";

  # temporary fix for nix versions that have the transitive follows bug
  # see https://github.com/NixOS/nix/issues/6013
  inputs.nixpkgs-2111 = { url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin"; };

  inputs.plutarch.url =
    "github:Plutonomicon/plutarch?rev=cb29ca64df4ed193d94a062e3fe26aa37e59b7bc";
  inputs.plutarch.inputs.nixpkgs.follows =
    "plutarch/haskell-nix/nixpkgs-unstable";

  outputs = inputs@{ self, nixpkgs, haskell-nix, plutarch, ... }:
    let
      supportedSystems = with nixpkgs.lib.systems.supported;
        tier1 ++ tier2 ++ tier3;

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [ haskell-nix.overlay ];
          inherit (haskell-nix) config;
        };

      nixpkgsFor' = system:
        import nixpkgs {
          inherit system;
          inherit (haskell-nix) config;
        };

      ghcVersion = "ghc921";

      projectFor = system:
        let pkgs = nixpkgsFor system;
        in let pkgs' = nixpkgsFor' system;
        in (nixpkgsFor system).haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = ghcVersion;
          inherit (plutarch) cabalProjectLocal;
          extraSources = plutarch.extraSources ++ [
            {
              src = inputs.plutarch;
              subdirs = [ "." "plutarch-test" "plutarch-extra" ];
            }
            {
              src = inputs.apropos-tx;
              subdirs = [ "." ];
            }
          ];
          modules = [ (plutarch.haskellModule system) ];
          shell = {
            withHoogle = true;

            exactDeps = true;

            # We use the ones from Nixpkgs, since they are cached reliably.
            # Eventually we will probably want to build these with haskell.nix.
            nativeBuildInputs = [
              pkgs'.git
              pkgs'.haskellPackages.apply-refact
              pkgs'.fd
              pkgs'.cabal-install
              pkgs'.haskell.packages."${ghcVersion}".hlint
              pkgs'.haskellPackages.cabal-fmt
              pkgs'.nixpkgs-fmt
              pkgs'.graphviz
            ];

            inherit (plutarch) tools;

            additional = ps: [
              ps.plutarch
              ps.plutarch-test
              ps.apropos-tx
              ps.plutarch-extra
            ];
          };
        };

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          inherit (pkgs.haskell-nix.tools ghcVersion {
            inherit (plutarch.tools) fourmolu hlint;
          })
            fourmolu hlint;
        in pkgs.runCommand "format-check" {
          nativeBuildInputs = [
            pkgs'.git
            pkgs'.fd
            pkgs'.haskellPackages.cabal-fmt
            pkgs'.nixpkgs-fmt
            fourmolu
            hlint
          ];
        } ''
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          cd ${self}
          make format_check
          mkdir $out
        '';
    in {
      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      packages = perSystem (system: self.flake.${system}.packages);
      checks = perSystem (system:
        self.flake.${system}.checks // {
          formatCheck = formatCheckFor system;
        });
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test" {
          checksss = builtins.attrValues self.checks.${system};
        } ''
          echo $checksss
          touch $out
        '');
      devShell = perSystem (system: self.flake.${system}.devShell);
      defaultPackage =
        perSystem (system: self.flake.${system}.packages."agora:lib:agora");
    };
}
