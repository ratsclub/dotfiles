{ inputs, self, ... }:

{
  nix.registry.nixpkgs.flake = inputs.nixpkgs;
  nix.registry.self.flake = self;
}
