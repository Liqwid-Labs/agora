{
  description = "agora";

  inputs.haskell-nix.url =
    "github:input-output-hk/haskell.nix?rev=4aeeba8d713d0b98c92c8c717df24da17d463c1d";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  inputs.haskell-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";

  # Temp workaround for Nix issue.
  inputs.nixpkgs-2111.url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin";

  inputs.plutus.url =
    "github:input-output-hk/plutus?rev=65bad0fd53e432974c3c203b1b1999161b6c2dce";

  inputs.plutarch.url =
    "github:Plutonomicon/plutarch?rev=1fd4db27152625184e559cfb465d225a0995a56b";

  inputs.goblins.url =
    "github:input-output-hk/goblins?rev=cde90a2b27f79187ca8310b6549331e59595e7ba";
  inputs.goblins.flake = false;

  inputs.plutus-extra.url =
    "github:Liqwid-Labs/plutus-extra?rev=bfeb0d2bb1bc18f147e58c200db2022f5c75eb60";
  inputs.plutus-extra.flake = false; # Could we set this to true?

  inputs.cardano-node.url =
    "github:input-output-hk/cardano-node?rev=b6ca519f97a0e795611a63174687e6bb70c9f752";
  inputs.cardano-node.flake = false;

  inputs.cardano-wallet.url =
    "github:j-mueller/cardano-wallet?rev=760140e238a5fbca61d1b286d7a80ece058dc729";
  inputs.cardano-wallet.flake = false;

  inputs.purescript-bridge.url =
    "github:input-output-hk/purescript-bridge?rev=366fc70b341e2633f3ad0158a577d52e1cd2b138";
  inputs.purescript-bridge.flake = false;

  inputs.servant-purescript.url =
    "github:input-output-hk/servant-purescript?rev=ebea59c7bdfc0338d83fca772b9a57e28560bcde";
  inputs.servant-purescript.flake = false;

  inputs.plutus-apps.url =
    "github:input-output-hk/plutus-apps?rev=34fe6eeff441166fee0cd0ceba68c1439f0e93d2";
  inputs.plutus-apps.flake = false;

  inputs.cardano-addresses.url =
    "github:input-output-hk/cardano-addresses?rev=d2f86caa085402a953920c6714a0de6a50b655ec";
  inputs.cardano-addresses.flake = false;

  inputs.optparse-applicative.url =
    "github:input-output-hk/optparse-applicative?rev=7497a29cb998721a9068d5725d49461f2bba0e7a";
  inputs.optparse-applicative.flake = false;

  inputs.ouroboros-network.url =
    "github:input-output-hk/ouroboros-network?rev=d613de3d872ec8b4a5da0c98afb443f322dc4dab";
  inputs.ouroboros-network.flake = false;

  inputs.cardano-ledger-specs.url =
    "github:input-output-hk/cardano-ledger-specs?rev=bf008ce028751cae9fb0b53c3bef20f07c06e333";
  inputs.cardano-ledger-specs.flake = false;

  inputs.iohk-monitoring-framework.url =
    "github:input-output-hk/iohk-monitoring-framework?rev=46f994e216a1f8b36fe4669b47b2a7011b0e153c";
  inputs.iohk-monitoring-framework.flake = false;

  inputs.cardano-prelude.url =
    "github:input-output-hk/cardano-prelude?rev=fd773f7a58412131512b9f694ab95653ac430852";
  inputs.cardano-prelude.flake = false;

  inputs.cardano-base.url =
    "github:input-output-hk/cardano-base?rev=654f5b7c76f7cc57900b4ddc664a82fc3b925fb0";
  inputs.cardano-base.flake = false;

  inputs.cardano-crypto.url =
    "github:input-output-hk/cardano-crypto?rev=f73079303f663e028288f9f4a9e08bcca39a923e";
  inputs.cardano-crypto.flake = false;

  inputs.flat.url =
    "github:Quid2/flat?rev=d32c2c0c0c3c38c41177684ade9febe92d279b06";
  inputs.flat.flake = false;

  inputs.Win32-network.url =
    "github:input-output-hk/Win32-network?rev=3825d3abf75f83f406c1f7161883c438dac7277d";
  inputs.Win32-network.flake = false;

  outputs = inputs@{ self, nixpkgs, haskell-nix, plutus, ... }:
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

      deferPluginErrors = true;
      plutarch-development = true;

      projectFor = system:
        let pkgs = nixpkgsFor system;
        in (nixpkgsFor system).haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc8107";
          cabalProjectFileName = "cabal.project";

          # This essentially replaces 'cabal-haskell.nix.project'
          extraSources = [
            {
              src = inputs.cardano-prelude;
              subdirs = [ "cardano-prelude" "cardano-prelude-test" ];
            }
            {
              src = inputs.cardano-base;
              subdirs = [
                "base-deriving-via"
                "binary"
                "binary/test"
                "cardano-crypto-class"
                "cardano-crypto-praos"
                "cardano-crypto-tests"
                "measures"
                "orphans-deriving-via"
                "slotting"
                "strict-containers"
              ];
            }

            {
              src = inputs.iohk-monitoring-framework;
              subdirs = [
                "iohk-monitoring"
                "tracer-transformers"
                "contra-tracer"
                "plugins/backend-aggregation"
                "plugins/backend-ekg"
                "plugins/backend-monitoring"
                "plugins/backend-trace-forwarder"
                "plugins/scribe-systemd"
              ];
            }
            {
              src = inputs.cardano-ledger-specs;
              subdirs = [
                "byron/ledger/impl"
                "cardano-ledger-core"
                "cardano-protocol-tpraos"
                "eras/alonzo/impl"
                "eras/byron/chain/executable-spec"
                "eras/byron/crypto"
                "eras/byron/crypto/test"
                "eras/byron/ledger/executable-spec"
                "eras/byron/ledger/impl/test"
                "eras/shelley/impl"
                "eras/shelley-ma/impl"
                "eras/shelley/chain-and-ledger/executable-spec"
                "eras/shelley/test-suite"
                "shelley/chain-and-ledger/shelley-spec-ledger-test"
                "libs/non-integral"
                "libs/small-steps"
                "libs/cardano-ledger-pretty"
                "semantics/small-steps-test"
              ];
            }
            {
              src = inputs.ouroboros-network;
              subdirs = [
                "monoidal-synchronisation"
                "typed-protocols"
                "typed-protocols-cborg"
                "typed-protocols-examples"
                "ouroboros-network"
                "ouroboros-network-testing"
                "ouroboros-network-framework"
                "ouroboros-consensus"
                "ouroboros-consensus-byron"
                "ouroboros-consensus-cardano"
                "ouroboros-consensus-shelley"
                "io-sim"
                "io-classes"
                "network-mux"
                "ntp-client"
              ];
            }
            {
              src = inputs.servant-purescript;
              subdirs = [ "." ];
            }
            {
              src = inputs.purescript-bridge;
              subdirs = [ "." ];
            }
            {
              src = inputs.plutarch;
              subdirs = [ "." "plutarch-benchmark" ];
            }
            {
              src = inputs.cardano-addresses;
              subdirs = [ "core" "command-line" ];
            }
            {
              src = inputs.goblins;
              subdirs = [ "." ];
            }
            {
              src = inputs.optparse-applicative;
              subdirs = [ "." ];
            }
            {
              src = inputs.cardano-crypto;
              subdirs = [ "." ];
            }
            {
              src = inputs.Win32-network;
              subdirs = [ "." ];
            }
            {
              src = inputs.flat;
              subdirs = [ "." ];
            }
            {
              src = inputs.cardano-wallet;
              subdirs = [
                "lib/text-class"
                "lib/strict-non-empty-containers"
                "lib/core"
                "lib/test-utils"
                "lib/numeric"
                "lib/launcher"
                "lib/core-integration"
                "lib/cli"
                "lib/dbvar"
                "lib/shelley"
              ];
            }

            {
              src = inputs.plutus-apps;
              subdirs = [
                "doc"
                "freer-extras"
                "playground-common"
                "plutus-chain-index"
                "plutus-chain-index-core"
                "plutus-contract"
                "plutus-ledger"
                "plutus-ledger-constraints"
                "plutus-pab"
                "plutus-playground-server"
                "plutus-use-cases"
                "quickcheck-dynamic"
                "web-ghc"
              ];
            }
            {
              src = inputs.cardano-node;
              subdirs =
                [ "cardano-api" "cardano-node" "cardano-cli" "cardano-config" ];
            }
            {
              src = inputs.plutus-extra;
              subdirs = [
                "tasty-plutus"
                "plutus-pretty"
                "plutus-numeric"
                "plutus-extra"
                "plutus-golden"
                "plutus-laws"
                "quickcheck-plutus-instances"

              ];
            }
            {
              src = inputs.plutus;
              subdirs = [
                "plutus-benchmark"
                "plutus-core"
                "plutus-errors"
                "plutus-ledger-api"
                "plutus-metatheory"
                "plutus-tx"
                "plutus-tx-plugin"
                "prettyprinter-configurable"
                "word-array"
                "stubs/plutus-ghc-stub"
              ];
            }
          ];
          modules = [{
            packages = {

              plutarch.flags.development = plutarch-development;
              marlowe.flags.defer-plugin-errors = deferPluginErrors;
              plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;
              plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;
              plutus-contract.flags.defer-plugin-errors = deferPluginErrors;
              cardano-crypto-praos.components.library.pkgconfig =
                nixpkgs.lib.mkForce
                [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
              cardano-crypto-class.components.library.pkgconfig =
                nixpkgs.lib.mkForce
                [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
            };
          }];
          shell = {
            withHoogle = true;

            exactDeps = true;

            nativeBuildInputs = with pkgs; [
              cabal-install
              hlint
              haskellPackages.fourmolu
              nixfmt
              haskellPackages.cabal-fmt
              haskellPackages.apply-refact
              haskellPackages.record-dot-preprocessor
              entr
              gnumake

              graphviz
            ];

            additional = ps: [
              ps.plutarch
              ps.plutarch-benchmark
              ps.plutus-ledger
              ps.plutus-extra
            ];

            tools.haskell-language-server = { };
          };
        };
    in {
      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      packages = perSystem (system: self.flake.${system}.packages);
      checks = perSystem (system: self.flake.${system}.checks);
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test" {
          nativeBuildInputs = builtins.attrValues self.checks.${system};
        } "touch $out");
      apps = perSystem (system: self.flake.${system}.apps);
      devShell = perSystem (system:
        self.flake.${system}.devShell.overrideAttrs (oldAttrs: {
          buildInputs = (nixpkgsFor system).lib.unique oldAttrs.buildInputs;
        }));
      defaultPackage =
        perSystem (system: self.flake.${system}.packages."agora:lib:agora");
    };
}
