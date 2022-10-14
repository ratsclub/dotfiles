{ inputs, ... }:

let
  inherit (inputs)
    hardware
    nixpkgs
    ;
in
{
  magnus = nixpkgs.lib.nixosSystem rec {
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ inputs.nur.overlay ];
    };
    modules = [ ./magnus ];
    specialArgs = { inherit inputs; };
  };
}
