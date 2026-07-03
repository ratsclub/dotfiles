# Build a home-manager configuration with our common package set and args.
{ inputs, self }:

let
  mkPkgs = import ./mk-pkgs.nix;
in
{ system, modules }:
inputs.homeManager.lib.homeManagerConfiguration {
  pkgs = mkPkgs {
    inherit (inputs) nixpkgs;
    inherit system;
    overlays = [ self.overlays.default ];
  };
  inherit modules;
  extraSpecialArgs = {
    inherit inputs self;
  };
}
