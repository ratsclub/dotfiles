{ config, inputs, pkgs, ... }:

{
  nix = {
    package = pkgs.nix;
    nixPath = [
      "nixpkgs=${inputs.nixpkgs}"
    ];

    gc = {
      automatic = true;
      options = "--delete-older-than 1w";
    };

    registry.nixpkgs.flake = inputs.nixpkgs;
    settings = {
      experimental-features = "nix-command flakes";
      auto-optimise-store = true;
      trusted-users = [ "root" "@wheel" ];
    };
  };
}
