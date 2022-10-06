{
  description = "My Nix configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # emacs-related
    emacs = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    doom-emacs = {
      url = "github:doomemacs/doomemacs";
      flake = false;
    };
    nixDoomEmacs = {
      url = "github:nix-community/nix-doom-emacs";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.doom-emacs.follows = "doom-emacs";
      inputs.emacs-overlay.follows = "emacs";
    };

    homeManager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixColors.url = "github:misterio77/nix-colors";
    nur.url = "github:nix-community/nur";
  };

  outputs = { self, ... }@inputs: {
    homeConfigurations = import ./home { inherit inputs; };
  };
}
