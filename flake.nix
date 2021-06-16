{
  description = "My NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/e92f5bb79e8c72b6afd64d3b6e4614669fcc1518";
      inputs.nixpkgs.follows = "unstable";
    };
  };

  outputs = { self, home-manager, nixpkgs, ... }@inputs:
    let
      username = "ratsclub";
    in
    rec
    {
      # nix build #.nixosConfigurations.thinkpad
      nixosConfigurations.thinkpad = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = [
          ./hosts/thinkpad
          ./home-manager/home.nix
          home-manager.nixosModules.home-manager
        ];
        specialArgs = { inherit inputs system; };
      };
    };
}
