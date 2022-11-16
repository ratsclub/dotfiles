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

  nix = {
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
