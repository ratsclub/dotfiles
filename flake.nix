{
  description = "My NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "unstable";
    };

    nixos-hardware.url = github:NixOS/nixos-hardware/master;

    nur.url = github:nix-community/NUR;
  };

  outputs = { self, home-manager, nur, nixpkgs, nixos-hardware, ... }@inputs:
    let
      username = "ratsclub";
    in
    rec
    {
      # nix build #.nixosConfigurations.thinkpad
      nixosConfigurations.thinkpad = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = [
          { nixpkgs.overlays = [ nur.overlay ]; }
          ./hosts/thinkpad
          ./home-manager/home.nix
          home-manager.nixosModules.home-manager
          nixos-hardware.nixosModules.lenovo-thinkpad-t495
        ];
        specialArgs = { inherit inputs system; };
      };
    };
}
