{
  description = "My NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    agenix.url = "github:ryantm/agenix";
    home-manager.url = "github:nix-community/home-manager/release-21.05";
    nixos-hardware.url = github:NixOS/nixos-hardware/master;
    nur.url = github:nix-community/NUR;
  };

  outputs = { self, home-manager, nur, nixpkgs, nixos-hardware, agenix, ... }@inputs:
    {
      # nix build #.nixosConfigurations.earth
      nixosConfigurations.earth =
        let
          system = "x86_64-linux";
        in
          nixpkgs.lib.nixosSystem rec {
            inherit system;
            modules = [

              # configuration.nix
              ./hosts/earth

              home-manager.nixosModules.home-manager
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.ratsclub = import ./home-manager/home.nix {
                  inherit inputs system;
                  pkgs = import nixpkgs {
                    inherit system;
                    overlays = [
                      nur.overlay
                    ];
                    config = {
                      allowUnfree = true;
                    };
                  };
                };
              }

              # hardware
              nixos-hardware.nixosModules.lenovo-thinkpad-t495

              # agenix
              agenix.nixosModules.age
            ];
            specialArgs = { inherit inputs system; };
          };
    };
}
