{
  description = "My Nix configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    small.url = "github:nixos/nixpkgs/nixos-unstable-small";
    stable.url = "github:nixos/nixpkgs/nixos-24.05";

    homeManager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hardware.url = "github:nixos/nixos-hardware";
    nur.url = "github:nix-community/nur";
  };

  outputs = { self, ... }@inputs:
    let
      inherit (self) outputs;
      forAllSystems = inputs.nixpkgs.lib.genAttrs [
        "aarch64-linux"
        "x86_64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
    in
    {
      packages = forAllSystems
        (system:
          let pkgs = import inputs.nixpkgs { inherit system; };
          in import ./pkgs { inherit pkgs; });

      overlays = import ./overlays { inherit (inputs) nixpkgs small; };
      homeConfigurations = import ./home { inherit inputs self; };
      nixosConfigurations = import ./hosts { inherit inputs outputs; };
      nixosModules = import ./modules;
      templates = import ./templates;
    };
}
