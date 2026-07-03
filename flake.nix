{
  description = "My Nix configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    small.url = "github:nixos/nixpkgs/nixos-unstable-small";
    stable.url = "github:nixos/nixpkgs/nixos-26.05";

    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "stable";
    };

    homeManager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self, ... }@inputs:
    let
      inherit (self) outputs;
      forAllSystems = inputs.nixpkgs.lib.genAttrs [
        "aarch64-linux"
        "x86_64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      mkPkgs = import ./lib/mk-pkgs.nix;
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = mkPkgs {
            inherit (inputs) nixpkgs;
            inherit system;
          };
        in
        import ./pkgs { inherit pkgs; }
      );

      formatter = forAllSystems (
        system:
        (mkPkgs {
          inherit (inputs) nixpkgs;
          inherit system;
        }).nixfmt-tree
      );

      overlays = import ./overlays { inherit (inputs) nixpkgs small; };
      homeConfigurations = import ./home { inherit inputs self; };
      nixosConfigurations = import ./hosts { inherit inputs outputs; };
      nixosModules = import ./modules;
      templates = import ./templates;
      devShells = import ./devShells { inherit inputs; };
    };
}
