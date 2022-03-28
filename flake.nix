{
  description = "agora";

  inputs.nixpkgs.follows = "plutarch/nixpkgs";
  inputs.haskell-nix.follows = "plutarch/haskell-nix";
  # temporary fix for nix versions that have the transitive follows bug
  # see https://github.com/NixOS/nix/issues/6013
  inputs.nixpkgs-2111 = { url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin"; };

  inputs.plutarch.url = "github:peter-mlabs/plutarch/liqwid/extra";
  inputs.plutarch.inputs.nixpkgs.follows =
    "plutarch/haskell-nix/nixpkgs-unstable";

  # Follows jhodgdev's forks of apropos and apropos-tx, as these 
  # are not constrained to `base ^>= 4.14`. Once these are merged 
  # to their respective master branches, we should change the 
  # inputs to follow a commit on those master branches. For more 
  # info, see: https://github.com/mlabs-haskell/apropos-tx/pull/37 
  inputs.apropos-tx.url =
    "github:jhodgdev/apropos-tx?rev=582496d0dfb88ce007bb0d2a2dcbc72ea0bb1cd1";
  inputs.apropos-tx.inputs.nixpkgs.follows =
    "plutarch/haskell-nix/nixpkgs-unstable";
  inputs.apropos.url =
    "github:jhodgdev/apropos?rev=c6c580aeab8b5c2a6512a49823dd17936e87b70a";


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
              subdirs =
                [ "." "plutarch-test" "plutarch-extra" "plutarch-numeric" ];
            }
            {
              src = inputs.apropos-tx;
              subdirs = [ "." ];
            }
            {
              src = inputs.apropos;
              subdirs = [ "." ];
            }
          ];
          modules = [ (plutarch.haskellModule system) ];
          shell = {
            withHoogle = true;

            exactDeps = true;

            # We use the ones from Nixpkgs, since they are cached reliably.
            # Eventually we will probably want to build these with haskell.nix.
            nativeBuildInputs = with pkgs'; [
              entr
              haskellPackages.apply-refact
              git
              fd
              cabal-install
              haskell.packages."${ghcVersion}".hlint
              haskellPackages.cabal-fmt
              nixpkgs-fmt
              graphviz
            ];

            inherit (plutarch) tools;

            additional = ps: [
              ps.plutarch
              ps.tasty-quickcheck
              ps.apropos-tx
              ps.plutarch-extra
              ps.plutarch-numeric
              ps.plutarch-test
              ps.apropos
            ];
          };
        };

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
        in pkgs.runCommand "format-check" {
          nativeBuildInputs = [
            pkgs'.git
            pkgs'.fd
            pkgs'.haskellPackages.cabal-fmt
            pkgs'.nixpkgs-fmt
            (pkgs.haskell-nix.tools ghcVersion {
              inherit (plutarch.tools) fourmolu;
            }).fourmolu
          ];
        } ''
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          cd ${self}
          make format_check || (echo "    Please run 'make format'" ; exit 1)
          mkdir $out
        '';
    in {
      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      packages = perSystem (system: self.flake.${system}.packages);

      # Define what we want to test
      checks = perSystem (system:
        self.flake.${system}.checks // {
          formatCheck = formatCheckFor system;
          agora = self.flake.${system}.packages."agora:lib:agora";
          agora-test = self.flake.${system}.packages."agora:test:agora-test";
        });
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test" {
          checksss = builtins.attrValues self.checks.${system};
        } ''
          echo $checksss
          touch $out
        '');
      devShell = perSystem (system: self.flake.${system}.devShell);
    };
}
