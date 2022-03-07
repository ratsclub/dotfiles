{ config
, inputs
, pkgs
, nixpkgs
, ...
}:

{
  nix = {
    package = pkgs.nixUnstable;

    nixPath = [
      "nixpkgs=/etc/nix/channels/nixpkgs"
      "home-manager=/etc/nix/channels/home-manager"
    ];

    gc = {
      automatic = true;
      options = "--delete-older-than 2d";
    };

    registry.nixpkgs.flake = nixpkgs;
    settings = {
      experimental-features = "nix-command flakes";
      auto-optimise-store = true;
    };
  };
}
