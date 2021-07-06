{
  description = "My NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";
    home-manager.url = "github:nix-community/home-manager/release-21.05";
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
      nixosConfigurations.t495 = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = [
          {
            nixpkgs.overlays = [
              nur.overlay
            ];
          }
          ./hosts/thinkpad
          ./home-manager/home.nix
          home-manager.nixosModules.home-manager
          nixos-hardware.nixosModules.lenovo-thinkpad-t495
        ];
        specialArgs = { inherit inputs system; };
      };
    };
}
