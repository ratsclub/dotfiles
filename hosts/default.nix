{ inputs, outputs, ... }:

let
  inherit (inputs)
    nixpkgs
    stable
    small
    ;

  mkPkgs = import ../lib/mk-pkgs.nix;
in
{
  catarina = nixpkgs.lib.nixosSystem rec {
    system = "x86_64-linux";
    pkgs = mkPkgs {
      inherit nixpkgs system;
      overlays = [ outputs.overlays.default ];
    };
    modules = [ ./catarina ];
    specialArgs = { inherit inputs; };
  };

  joan = nixpkgs.lib.nixosSystem rec {
    system = "x86_64-linux";
    pkgs = mkPkgs {
      inherit nixpkgs system;
      overlays = [ outputs.overlays.default ];
    };
    modules = [ ./joan ];
    specialArgs = { inherit inputs; };
  };
}
