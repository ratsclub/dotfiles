{
  description = "My Nix configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    stable.url = "github:nixos/nixpkgs/nixos-22.05";

    # emacs-related
    emacs = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixDoomEmacs.url = "github:nix-community/nix-doom-emacs";

    homeManager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hardware.url = "github:nixos/nixos-hardware";
    nixColors.url = "github:misterio77/nix-colors";
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

      overlays = import ./overlays;
      homeConfigurations = import ./home { inherit inputs self; };
      nixosConfigurations = import ./hosts { inherit inputs outputs; };
      templates = import ./templates;
    };
}
