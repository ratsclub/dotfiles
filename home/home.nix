{ config, inputs, username, ... }:

let
  inherit (inputs) nix-colors;
  inherit (config.meta) username;
in
{
  colorscheme = nix-colors.colorSchemes.dracula;
  fonts.fontconfig.enable = true;

  home = {
    inherit username;
    homeDirectory = "/home/${username}";
    stateVersion = "20.09";
  };
}
