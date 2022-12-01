{ self, ... }:
{
  perSystem = { config, pkgs', self', inputs, system, ... }:
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
          "${self.inputs.plutarch-numeric}"
          "${self.inputs.plutarch-quickcheck}"
          "${self.inputs.plutarch-context-builder}"
          "${self.inputs.liqwid-plutarch-extra}"
          "${self.inputs.liqwid-script-export}"
          "${self.inputs.liqwid-script-export.inputs.ply}/ply-core"
          "${self.inputs.liqwid-script-export.inputs.ply}/ply-plutarch"
        ];
      };
    };
}
