{ config, pkgs, inputs, ... }:

let
  inherit (inputs) nixColors nixpkgs;
in
{
  imports = [
    inputs.nixColors.homeManagerModule
  ];

  colorscheme = nixColors.colorSchemes.monokai;
  fonts.fontconfig.enable = true;

  age.identityPaths = [
    "${config.home.homeDirectory}/.ssh/id_ed25519"
  ];

  nix = {
    package = pkgs.nixUnstable;
    registry.nixpkgs.flake = nixpkgs;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  home = {
    username = "victor";
    homeDirectory = "/home/victor";
    stateVersion = "22.11";
  };
}
