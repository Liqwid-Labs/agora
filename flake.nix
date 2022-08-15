{
  description = "agora";

  inputs = {
    nixpkgs.follows = "plutarch/nixpkgs";
    nixpkgs-latest.url = "github:NixOS/nixpkgs?rev=cf63df0364f67848083ff75bc8ac9b7ca7aa5a01";
    # temporary fix for nix versions that have the transitive follows bug
    # see https://github.com/NixOS/nix/issues/6013
    nixpkgs-2111 = { url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin"; };

    haskell-nix-extra-hackage.follows = "plutarch/haskell-nix-extra-hackage";
    haskell-nix.follows = "plutarch/haskell-nix";
    iohk-nix.follows = "plutarch/iohk-nix";
    haskell-language-server.follows = "plutarch/haskell-language-server";

    # Plutarch and its friends
    plutarch = {
      url = "github:Plutonomicon/plutarch-plutus?ref=staging";

      inputs.emanote.follows =
        "plutarch/haskell-nix/nixpkgs-unstable";
      inputs.nixpkgs.follows =
        "plutarch/haskell-nix/nixpkgs-unstable";
    };

    plutarch-numeric.url =
      "github:Liqwid-Labs/plutarch-numeric?ref=main";
    plutarch-safe-money.url =
      "github:Liqwid-Labs/plutarch-safe-money?ref=main";
    liqwid-plutarch-extra.url =
      "github:Liqwid-Labs/liqwid-plutarch-extra?ref=main";
    plutarch-quickcheck.url =
      "github:liqwid-labs/plutarch-quickcheck?ref=staging";
    plutarch-context-builder.url =
      "github:Liqwid-Labs/plutarch-context-builder?ref=staging";
    plutarch-script-export.url =
      "github:Liqwid-Labs/plutarch-script-export?ref=main";

    liqwid-nix.url = "github:Liqwid-Labs/liqwid-nix?ref=main";
  };

  outputs = inputs@{ liqwid-nix, ... }:
    let
      benchCheckOverlay = self: super: {
        toFlake =
          let
            inherit (self) inputs perSystem pkgsFor';
            flake = super.toFlake or { };
            name = "benchCheck";
          in
          flake // {
            checks = perSystem (system:
              flake.checks.${system} // {
                ${name} =
                  let
                    pkgs' = pkgsFor' system;
                    bench = flake.packages.${system}."agora:bench:agora-bench";
                  in
                  pkgs'.runCommand name
                    {
                      nativeBuildInputs = [ pkgs'.diffutils ];
                    } ''
                    export LC_CTYPE=C.UTF-8
                    export LC_ALL=C.UTF-8
                    export LANG=C.UTF-8
                    cd ${inputs.self}
                    ${bench}/bin/agora-bench | diff bench.csv - \
                      || (echo "bench.csv is outdated"; exit 1)
                    mkdir "$out"
                  '';
              });
          };
      };
    in
    (liqwid-nix.buildProject
      {
        inherit inputs;
        src = ./.;
      }
      [
        liqwid-nix.haskellProject
        liqwid-nix.plutarchProject
        (liqwid-nix.addDependencies [
          "${inputs.plutarch-numeric}"
          "${inputs.plutarch-safe-money}"
          "${inputs.plutarch-quickcheck}"
          "${inputs.plutarch-context-builder}"
          "${inputs.liqwid-plutarch-extra}"
          "${inputs.plutarch-script-export}"
        ])
        (liqwid-nix.enableFormatCheck [
          "-XQuasiQuotes"
          "-XTemplateHaskell"
          "-XTypeApplications"
          "-XImportQualifiedPost"
          "-XPatternSynonyms"
          "-XOverloadedRecordDot"
        ])
        liqwid-nix.enableLintCheck
        liqwid-nix.enableCabalFormatCheck
        liqwid-nix.enableNixFormatCheck
        liqwid-nix.addBuildChecks
        (liqwid-nix.addCommandLineTools (pkgs: _: [
          pkgs.haskellPackages.hasktags
        ]))
        benchCheckOverlay
      ]
    ).toFlake;
}
