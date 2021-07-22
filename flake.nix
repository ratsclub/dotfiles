{
  description = "My NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";
    agenix.url = "github:ryantm/agenix";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/release-21.05";
    nixos-hardware.url = github:NixOS/nixos-hardware/master;
    nur.url = github:nix-community/NUR;
  };

  outputs =
    { self
    , home-manager
    , nur
    , nixpkgs
    , nixos-hardware
    , agenix
    , ...
    }@inputs:
      {
        # nix build #.nixosConfigurations.earth
        nixosConfigurations.earth =
          let
            system = "x86_64-linux";
          in
            nixpkgs.lib.nixosSystem rec {
              inherit system;
              modules = [
                {
                  nixpkgs.overlays = [
                    nur.overlay
                  ];
                }
                agenix.nixosModules.age
                ./hosts/earth
                ./home-manager/home.nix
                home-manager.nixosModules.home-manager
                nixos-hardware.nixosModules.lenovo-thinkpad-t495
              ];
              specialArgs = { inherit inputs system; };
            };

        # nix build #.nixosConfigurations.mars
        nixosConfigurations.mars =
          let
            system = "x86_64-linux";
          in
            nixpkgs.lib.nixosSystem rec {
              inherit system;
              modules = [
                {
                  nixpkgs.overlays = [
                    nur.overlay
                  ];
                }
                ./hosts/mars
                ./home-manager/home.nix
                home-manager.nixosModules.home-manager
                nixos-hardware.nixosModules.lenovo-thinkpad-x230
              ];
              specialArgs = { inherit inputs system; };
            };

        nixosConfigurations.moon =
          let
            system = "aarch64-linux";
          in
            nixpkgs.lib.nixosSystem rec {
              inherit system;
              modules = [];
              specialArgs = { inherit inputs system; };
            };
      };
}
