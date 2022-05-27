{
  description = "agora";

  inputs.nixpkgs.follows = "plutarch/nixpkgs";
  inputs.haskell-nix.follows = "plutarch/haskell-nix";
  # temporary fix for nix versions that have the transitive follows bug
  # see https://github.com/NixOS/nix/issues/6013
  inputs.nixpkgs-2111 = { url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin"; };

  # Plutarch and its friends
  inputs.plutarch.url =
    "github:liqwid-labs/plutarch/staging";
  inputs.plutarch.inputs.emanote.follows =
    "plutarch/haskell-nix/nixpkgs-unstable";
  inputs.plutarch.inputs.nixpkgs.follows =
    "plutarch/haskell-nix/nixpkgs-unstable";

  inputs.liqwid-plutarch-extra.url =
    "git+ssh://git@github.com/Liqwid-Labs/liqwid-plutarch-extra?ref=main";
  inputs.plutarch-numeric.url =
    "git+ssh://git@github.com/Liqwid-Labs/plutarch-numeric?ref=main";
  inputs.plutarch-safe-money.url =
    "git+ssh://git@github.com/Liqwid-Labs/plutarch-safe-money?ref=main";

  # Follows jhodgdev's forks of apropos and apropos-tx, as these
  # are not constrained to `base ^>= 4.14`. Once these are merged
  # to their respective master branches, we should change the
  # inputs to follow a commit on those master branches. For more
  # info, see: https://github.com/mlabs-haskell/apropos-tx/pull/37
  inputs.apropos-tx.url =
    "github:jhodgdev/apropos-tx?rev=4eca3fac23c339caee04ea6176e641a4b3857a25";
  inputs.apropos-tx.inputs.nixpkgs.follows =
    "plutarch/haskell-nix/nixpkgs-unstable";
  inputs.apropos.url =
    "github:mlabs-haskell/apropos?rev=3734bb3baa297ed990725a5ef14efcbb6a1c1c23";
  inputs.apropos.inputs.nixpkgs.follows =
    "plutarch/haskell-nix/nixpkgs-unstable";

  inputs.purescript-bridge.url =
    "github:mlabs-haskell/purescript-bridge?rev=8e6251e8b1f489748f5bbd9ca6384bcf8cefbbef";

  outputs = inputs@{ self, nixpkgs, haskell-nix, plutarch, purescript-bridge, ... }:
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
        in
        let pkgs' = nixpkgsFor' system;
        in
        (nixpkgsFor system).haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = ghcVersion;
          inherit (plutarch) cabalProjectLocal;
          extraSources = plutarch.extraSources ++ [
            {
              src = inputs.plutarch;
              subdirs = [
                "."
                "plutarch-extra"
              ];
            }
            {
              src = inputs.liqwid-plutarch-extra;
              subdirs = [ "." ];
            }
            {
              src = inputs.plutarch-numeric;
              subdirs = [ "." ];
            }
            {
              src = inputs.plutarch-safe-money;
              subdirs = [ "." ];
            }
            {
              src = inputs.apropos-tx;
              subdirs = [ "." ];
            }
            {
              src = inputs.apropos;
              subdirs = [ "." ];
            }
            {
              src = inputs.purescript-bridge;
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
              # plutarch
              ps.plutarch
              ps.liqwid-plutarch-extra
              ps.plutarch-numeric
              ps.plutarch-safe-money

              # purescript
              ps.purescript-bridge

              # testing
              ps.tasty-quickcheck
              ps.apropos-tx
              ps.apropos
              ps.apropos

            ];
          };
        };

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;

          inherit (pkgs.haskell-nix.tools ghcVersion {
            inherit (plutarch.tools) fourmolu;
          })
            fourmolu;
        in
        pkgs.runCommand "format-check"
          {
            nativeBuildInputs = [
              pkgs'.git
              pkgs'.fd
              pkgs'.haskellPackages.cabal-fmt
              pkgs'.nixpkgs-fmt
              fourmolu
              pkgs'.haskell.packages."${ghcVersion}".hlint
            ];
          } ''
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          cd ${self}
          make format_check || (echo "    Please run 'make format'" ; exit 1)
          find -name '*.hs' -not -path './dist*/*' -not -path './haddock/*' | xargs hlint
          mkdir $out
        '';

      benchCheckFor = system: agora-bench:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
        in
        pkgs.runCommand "bench-check"
          {
            bench = "${agora-bench}/bin/agora-bench";
            nativeBuildInputs = [
              pkgs'.diffutils
            ];
          } ''
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          cd ${self}
          make bench_check || (echo "    Please run 'make bench'" ; exit 1)
          mkdir $out 
        '';

    in
    {
      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      packages = perSystem (system:
        self.flake.${system}.packages // {
          haddock =
            let
              agora-doc = self.flake.${system}.packages."agora:lib:agora".doc;
              pkgs = nixpkgsFor system;
            in
            pkgs.runCommand "haddock-merge" { } ''
              cd ${self}
              mkdir $out
              cp -r ${agora-doc}/share/doc/* $out
            '';
        });

      # Define what we want to test
      checks = perSystem (system:
        self.flake.${system}.checks // {
          formatCheck = formatCheckFor system;
          benchCheck = benchCheckFor system self.flake.${system}.packages."agora:bench:agora-bench";
          agora = self.flake.${system}.packages."agora:lib:agora";
          agora-test = self.flake.${system}.packages."agora:test:agora-test";
        });
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test"
          {
            checksss = builtins.attrValues self.checks.${system};
          } ''
          echo $checksss
          touch $out
        '');
      devShell = perSystem (system: self.flake.${system}.devShell);
    };
}
