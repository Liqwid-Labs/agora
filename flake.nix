{
  description = "agora";

  inputs.nixpkgs.follows = "plutarch/nixpkgs";
  inputs.haskell-nix.follows = "plutarch/haskell-nix";
  inputs.nixpkgs-latest.url = "github:NixOS/nixpkgs?rev=a0a69be4b5ee63f1b5e75887a406e9194012b492";
  # temporary fix for nix versions that have the transitive follows bug
  # see https://github.com/NixOS/nix/issues/6013
  inputs.nixpkgs-2111 = { url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin"; };

  # Plutarch and its friends
  inputs.plutarch.url =
    "github:liqwid-labs/plutarch?rev=e7ef565645146e26e75ec29fe97122a74e52c6b7";
  inputs.plutarch.inputs.emanote.follows =
    "plutarch/haskell-nix/nixpkgs-unstable";
  inputs.plutarch.inputs.nixpkgs.follows =
    "plutarch/haskell-nix/nixpkgs-unstable";

  inputs.liqwid-plutarch-extra.url =
    "github:Liqwid-Labs/liqwid-plutarch-extra?ref=main";
  inputs.plutarch-numeric.url =
    "github:Liqwid-Labs/plutarch-numeric?ref=main";
  inputs.plutarch-safe-money.url =
    "github:Liqwid-Labs/plutarch-safe-money?ref=main";

  # Testing
  inputs.plutarch-quickcheck.url =
    "github:liqwid-labs/plutarch-quickcheck?ref=staging";
  inputs.plutarch-context-builder.url =
    "github:Liqwid-Labs/plutarch-context-builder?ref=main";

  outputs = inputs@{ self, nixpkgs, nixpkgs-latest, haskell-nix, plutarch, ... }:
    let
      supportedSystems = nixpkgs-latest.lib.systems.flakeExposed;

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      pkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ haskell-nix.overlay (import "${plutarch.inputs.iohk-nix}/overlays/crypto") ];
        # This only does bad things for us...
        # inherit (haskell-nix) config;
      };
      pkgsFor' = system: import nixpkgs-latest { inherit system; };

      fourmoluFor = system: (pkgsFor' system).haskell.packages.ghc922.fourmolu_0_6_0_0;

      defaultGhcVersion = "ghc923";

      nonReinstallablePkgs = [
        "array"
        "array"
        "base"
        "binary"
        "bytestring"
        "Cabal"
        "containers"
        "deepseq"
        "directory"
        "exceptions"
        "filepath"
        "ghc"
        "ghc-bignum"
        "ghc-boot"
        "ghc-boot"
        "ghc-boot-th"
        "ghc-compact"
        "ghc-heap"
        # "ghci"
        # "haskeline"
        "ghcjs-prim"
        "ghcjs-th"
        "ghc-prim"
        "ghc-prim"
        "hpc"
        "integer-gmp"
        "integer-simple"
        "mtl"
        "parsec"
        "pretty"
        "process"
        "rts"
        "stm"
        "template-haskell"
        "terminfo"
        "text"
        "time"
        "transformers"
        "unix"
        "Win32"
        "xhtml"
      ];

      haskellModules = [
        ({ config, pkgs, hsPkgs, ... }: {
          inherit nonReinstallablePkgs; # Needed for a lot of different things
          packages = {
            cardano-binary.doHaddock = false;
            cardano-binary.ghcOptions = [ "-Wwarn" ];
            cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
            cardano-crypto-class.doHaddock = false;
            cardano-crypto-class.ghcOptions = [ "-Wwarn" ];
            cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
            cardano-prelude.doHaddock = false; # somehow above options are not applied?
            cardano-prelude.ghcOptions = [ "-Wwarn" ];
            # Workaround missing support for build-tools:
            # https://github.com/input-output-hk/haskell.nix/issues/231
            plutarch-test.components.exes.plutarch-test.build-tools = [
              config.hsPkgs.hspec-discover
            ];
          };
        })
      ];

      myhackage = system: compiler-nix-name: plutarch.inputs.haskell-nix-extra-hackage.mkHackageFor system compiler-nix-name (
        [
          "${inputs.plutarch.inputs.flat}"
          "${inputs.plutarch.inputs.protolude}"
          "${inputs.plutarch.inputs.cardano-prelude}/cardano-prelude"
          "${inputs.plutarch.inputs.cardano-crypto}"
          "${inputs.plutarch.inputs.cardano-base}/binary"
          "${inputs.plutarch.inputs.cardano-base}/cardano-crypto-class"
          "${inputs.plutarch.inputs.plutus}/plutus-core"
          "${inputs.plutarch.inputs.plutus}/plutus-ledger-api"
          "${inputs.plutarch.inputs.plutus}/plutus-tx"
          "${inputs.plutarch.inputs.plutus}/prettyprinter-configurable"
          "${inputs.plutarch.inputs.plutus}/word-array"
          "${inputs.plutarch.inputs.secp256k1-haskell}"
          "${inputs.plutarch.inputs.plutus}/plutus-tx-plugin" # necessary for FFI tests

          # Custom deps as a consumer
          "${inputs.plutarch}"
          "${inputs.plutarch}/plutarch-extra"
          "${inputs.liqwid-plutarch-extra}"
          "${inputs.plutarch-numeric}"
          "${inputs.plutarch-safe-money}"
          "${inputs.plutarch-quickcheck}"
          "${inputs.plutarch-context-builder}"
        ]
      );

      applyDep = pkgs: o:
        let h = myhackage pkgs.system o.compiler-nix-name; in
        (plutarch.applyPlutarchDep pkgs o) // {
          modules = haskellModules ++ [ h.module ] ++ (o.modules or [ ]);
          extra-hackages = [ (import h.hackageNix) ] ++ (o.extra-hackages or [ ]);
          extra-hackage-tarballs = { _xNJUd_plutarch-hackage = h.hackageTarball; } // (o.extra-hackage-tarballs or { });
        };

      projectForGhc = compiler-nix-name: system:
        let pkgs = pkgsFor system; in
        let pkgs' = pkgsFor' system; in
        let pkgSet = pkgs.haskell-nix.cabalProject' (applyDep pkgs {
          src = ./.;
          inherit compiler-nix-name;
          modules = [ ];
          shell = {
            withHoogle = true;

            exactDeps = true;

            # We use the ones from Nixpkgs, since they are cached reliably.
            # Eventually we will probably want to build these with haskell.nix.
            nativeBuildInputs = [
              pkgs'.cabal-install
              pkgs'.hlint
              pkgs'.haskellPackages.cabal-fmt
              (fourmoluFor system)
              pkgs'.nixpkgs-fmt
              (plutarch.hlsFor compiler-nix-name system)
            ];
          };
        }); in
        pkgSet;

      projectFor = projectForGhc defaultGhcVersion;

      formatCheckFor = system:
        let
          pkgs' = pkgsFor' system;
        in
        pkgs'.runCommand "format-check"
          {
            nativeBuildInputs = [ pkgs'.haskellPackages.cabal-fmt pkgs'.nixpkgs-fmt (fourmoluFor system) pkgs'.hlint ];
          } ''
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          cd ${self}
          make format_check || (echo "    Please run 'make format'" ; exit 1)
          find -name '*.hs' -not -path './dist*/*' -not -path './haddock/*' | xargs hlint
          mkdir $out
        ''
      ;

      benchCheckFor = system: agora-bench:
        let
          pkgs = pkgsFor system;
          pkgs' = pkgsFor' system;
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
              pkgs = pkgsFor system;
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
          # benchCheck = benchCheckFor system self.flake.${system}.packages."agora:bench:agora-bench";
          agora = self.flake.${system}.packages."agora:lib:agora";
          agora-test = self.flake.${system}.packages."agora:test:agora-test";
          benchCheck = benchCheckFor system self.flake.${system}.packages."agora:bench:agora-bench";
        });
      check = perSystem (system:
        (pkgsFor system).runCommand "combined-test"
          {
            checksss = builtins.attrValues self.checks.${system};
          } ''
          echo $checksss
          touch $out
        '');
      devShell = perSystem (system: self.flake.${system}.devShell);
    };
}
