{ config, pkgs, inputs, ... }:

let
  inherit (inputs) nixpkgs;
in
{
  imports = [ ];

  fonts.fontconfig.enable = true;

  age.identityPaths = [
    "${config.home.homeDirectory}/.ssh/id_ed25519"
  ];

  nix = {
    package = pkgs.nixVersions.latest;
    registry.nixpkgs.flake = nixpkgs;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  home = {
    username = "victor";
    homeDirectory = "/home/victor";
    stateVersion = "22.11";
    sessionPath = [
      "$HOME/.local/bin"
    ];
  };
}
