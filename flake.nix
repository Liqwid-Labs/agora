{
  description = "agora";

  inputs = {
    nixpkgs.follows = "plutarch/nixpkgs";
    nixpkgs-latest.url = "github:NixOS/nixpkgs?rev=cf63df0364f67848083ff75bc8ac9b7ca7aa5a01";
    # temporary fix for nix versions that have the transitive follows bug
    # see https://github.com/NixOS/nix/issues/6013
    nixpkgs-2111 = { url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin"; };
    nixpkgs-2205 = { url = "github:NixOS/nixpkgs/22.05"; };

    haskell-nix-extra-hackage.follows = "plutarch/haskell-nix-extra-hackage";
    haskell-nix.follows = "plutarch/haskell-nix";
    iohk-nix.follows = "plutarch/iohk-nix";
    haskell-language-server.follows = "plutarch/haskell-language-server";

    # Plutarch and its friends
    plutarch = {
      url = "github:Plutonomicon/plutarch-plutus?ref=master";

      inputs.emanote.follows =
        "plutarch/haskell-nix/nixpkgs-unstable";
      inputs.nixpkgs.follows =
        "plutarch/haskell-nix/nixpkgs-unstable";
    };
    ply = {
      url = "github:mlabs-haskell/ply?ref=master";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.extra-hackage.follows = "haskell-nix-extra-hackage";
      inputs.iohk-nix.follows = "iohk-nix";
      inputs.plutarch.follows = "plutarch";
    };
    plutarch-numeric = {
      url = "github:Liqwid-Labs/plutarch-numeric?ref=main";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
      inputs.nixpkgs-2111.follows = "nixpkgs-2111";
      inputs.haskell-nix-extra-hackage.follows = "haskell-nix-extra-hackage";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.iohk-nix.follows = "iohk-nix";
      inputs.haskell-language-server.follows = "haskell-language-server";
      inputs.plutarch.follows = "plutarch";
      inputs.liqwid-nix.follows = "liqwid-nix";
    };
    liqwid-plutarch-extra = {
      url = "github:Liqwid-Labs/liqwid-plutarch-extra?ref=main";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
      inputs.nixpkgs-2111.follows = "nixpkgs-2111";
      inputs.nixpkgs-2205.follows = "nixpkgs-2205";
      inputs.haskell-nix-extra-hackage.follows = "haskell-nix-extra-hackage";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.iohk-nix.follows = "iohk-nix";
      inputs.haskell-language-server.follows = "haskell-language-server";
      inputs.plutarch.follows = "plutarch";
      inputs.plutarch-quickcheck.follows = "plutarch-quickcheck";
      inputs.plutarch-numeric.follows = "plutarch-numeric";
      inputs.plutarch-context-builder.follows = "plutarch-context-builder";
      inputs.ply.follows = "ply";
      inputs.liqwid-nix.follows = "liqwid-nix";
    };
    plutarch-quickcheck = {
      url = "github:liqwid-labs/plutarch-quickcheck?ref=staging";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
      inputs.nixpkgs-2111.follows = "nixpkgs-2111";
      inputs.haskell-nix-extra-hackage.follows = "haskell-nix-extra-hackage";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.iohk-nix.follows = "iohk-nix";
      inputs.haskell-language-server.follows = "haskell-language-server";
      inputs.plutarch.follows = "plutarch";
      inputs.liqwid-nix.follows = "liqwid-nix";
    };
    plutarch-context-builder = {
      url = "github:Liqwid-Labs/plutarch-context-builder?ref=main";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
      inputs.nixpkgs-2111.follows = "nixpkgs-2111";
      inputs.haskell-nix-extra-hackage.follows = "haskell-nix-extra-hackage";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.iohk-nix.follows = "iohk-nix";
      inputs.haskell-language-server.follows = "haskell-language-server";
      inputs.plutarch.follows = "plutarch";
      inputs.liqwid-nix.follows = "liqwid-nix";
    };
    liqwid-script-export = {
      url = "github:Liqwid-Labs/liqwid-script-export?ref=main";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
      inputs.nixpkgs-2111.follows = "nixpkgs-2111";
      inputs.haskell-nix-extra-hackage.follows = "haskell-nix-extra-hackage";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.iohk-nix.follows = "iohk-nix";
      inputs.haskell-language-server.follows = "haskell-language-server";
      inputs.plutarch.follows = "plutarch";
      inputs.ply.follows = "ply";
      inputs.liqwid-nix.follows = "liqwid-nix";
      inputs.plutarch-numeric.follows = "plutarch-numeric";
      inputs.liqwid-plutarch-extra.follows = "liqwid-plutarch-extra";
    };
    # Dependencies need addChecks, which was removed after this commit
    liqwid-nix.url = "github:Liqwid-Labs/liqwid-nix/f31230a055dbad1653c11784cc52ef07f40cafdb";
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
          "${inputs.plutarch-quickcheck}"
          "${inputs.plutarch-context-builder}"
          "${inputs.liqwid-plutarch-extra}"
          "${inputs.liqwid-script-export}"
          "${inputs.liqwid-script-export.inputs.ply}/ply-core"
          "${inputs.liqwid-script-export.inputs.ply}/ply-plutarch"
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
