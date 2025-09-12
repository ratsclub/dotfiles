{ inputs, self, ... }:

{
  nix.registry = {
    nixpkgs = {
      from = { type = "indirect"; id = "nixpkgs"; };
      flake = inputs.nixpkgs;
    };
    self = {
      from = { type = "indirect"; id = "self"; };
      flake = self;
    };
  };
}
