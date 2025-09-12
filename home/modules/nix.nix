{ inputs, ... }:

{
  nix.registry = {
    nixpkgs = {
      from = { type = "indirect"; id = "nixpkgs"; };
      flake = inputs.nixpkgs;
    };
  };
}
