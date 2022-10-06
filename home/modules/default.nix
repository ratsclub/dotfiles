{ config, pkgs, inputs, ... }:

let
  inherit (inputs) nixColors nixpkgs;
in
{
  colorscheme = nixColors.colorSchemes.monokai;
  fonts.fontconfig.enable = true;

  nix = {
    registry.nixpkgs.flake = nixpkgs;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    package = pkgs.nixUnstable;
  };

  home = {
    username = "victor";
    homeDirectory = "/home/victor";
    stateVersion = "22.11";
  };
}
