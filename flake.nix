{
  description = "My NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";
    home-manager = {
      url = "github:nix-community/home-manager/e92f5bb79e8c72b6afd64d3b6e4614669fcc1518";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, home-manager, nixpkgs, ... }@inputs: {

    # nix build #.nixosConfigurations.thinkpad
    nixosConfigurations.thinkpad = nixpkgs.lib.nixosSystem rec {
      system = "x86_64-linux";
      modules = [ ./hosts/thinkpad ];
      specialArgs = { inherit inputs system; };
    };

    homeConfigurations.home-linux = home-manager.lib.homeManagerConfiguration rec {
      configuration = ./home-manager/home.nix;
      system = "x86_64-linux";
      homeDirectory = "/home/ratsclub";
      username = "ratsclub";
    };
  };
}
