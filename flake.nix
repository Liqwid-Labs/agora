{
  description = "agora";

  nixConfig = {
    extra-experimental-features = [ "nix-command" "flakes" "ca-derivations" ];
    extra-substituters = [ "https://cache.iog.io" "https://public-plutonomicon.cachix.org" "https://mlabs.cachix.org" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=" ];
    allow-import-from-derivation = "true";
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]liqwid-nix \\e[0;5m\\]2.0 \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
    max-jobs = "auto";
    auto-optimise-store = "true";
  };

  inputs = {
    nixpkgs.follows = "liqwid-nix/nixpkgs";
    nixpkgs-latest.url = "github:NixOS/nixpkgs";

    liqwid-nix = {
      url = "github:Liqwid-Labs/liqwid-nix/liqwid-nix-2.0";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
    };

    ply.url = "github:emiflake/ply?ref=emiflake/add-missing-instance";
    plutarch-numeric.url = "github:Liqwid-Labs/plutarch-numeric/emiflake/liqwid-nix-2.0";
    plutarch-numeric.inputs.ply.follows = "ply";
    liqwid-plutarch-extra.url = "github:Liqwid-Labs/liqwid-plutarch-extra/emiflake/liqwid-nix-2.0";
    liqwid-plutarch-extra.inputs.ply.follows = "ply";
    plutarch-quickcheck.url = "github:liqwid-labs/plutarch-quickcheck/emiflake/liqwid-nix-2.0";
    plutarch-quickcheck.inputs.ply.follows = "ply";
    plutarch-context-builder.url = "github:Liqwid-Labs/plutarch-context-builder/emiflake/liqwid-nix-2.0";
    plutarch-context-builder.inputs.ply.follows = "ply";
    liqwid-script-export.url = "github:Liqwid-Labs/liqwid-script-export/emiflake/liqwid-nix-2.0";
    liqwid-script-export.inputs.ply.follows = "ply";
  };

  outputs = { self, liqwid-nix, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      imports = [
        liqwid-nix.onchain
        liqwid-nix.run
        ./.
      ];
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
      perSystem = { config, self', inputs', pkgs, system, ... }: { };
    };
}
