{
  description = "My NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    nixos-hardware.url = github:NixOS/nixos-hardware/master;
    emacs.url = "github:nix-community/emacs-overlay";
    nur.url = github:nix-community/NUR;
  };

  outputs = { self, home-manager, nur, nixpkgs, nixos-hardware, ... }@inputs:
    {
      nixosConfigurations =
        let
          system = "x86_64-linux";
        in
        {
          # nix build .nixosConfigurations.earth
          earth = nixpkgs.lib.nixosSystem {
            inherit system;
            modules = [

              # configuration.nix
              ./hosts/earth

              home-manager.nixosModules.home-manager
              {
                home-manager = {
                  useUserPackages = true;
                  users.ratsclub = import ./home-manager/gui.nix;
                  extraSpecialArgs = {
                    inherit inputs system;
                  };
                };
              }

              # hardware
              nixos-hardware.nixosModules.lenovo-thinkpad-t495
            ];
            specialArgs = { inherit inputs system; };
          };

          # nix build .nixosConfigurations.earth
          jupyter = nixpkgs.lib.nixosSystem {
            inherit system;
            modules = [

              # configuration.nix
              ./hosts/jupyter

              home-manager.nixosModules.home-manager
              {
                home-manager = {
                  useUserPackages = true;
                  users.ratsclub = import ./home-manager/cli.nix;
                  extraSpecialArgs = {
                    inherit inputs system;
                  };
                };
              }
            ];
            specialArgs = { inherit inputs system; };
          };
        };

      homeConfigurations.mars = home-manager.lib.homeManagerConfiguration rec {
        configuration = ./home-manager/cli.nix;
        system = "x86_64-linux";
        homeDirectory = "/home/victor";
        username = "victor";
        extraSpecialArgs = {
          inherit inputs system;
        };
      };
    };
}
