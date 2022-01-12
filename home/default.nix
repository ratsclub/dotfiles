{ username, inputs, ... }:

let
  inherit (inputs) nix-colors;
in
{
  imports = [
    ./chromium.nix
    ./cli
    ./firefox.nix
    ./gui.nix
    ./kitty.nix
    ./mako.nix
    ./neovim.nix
    ./newsboat.nix
    ./sway.nix
    ./vscodium.nix

    nix-colors.homeManagerModule
  ];

  colorscheme = nix-colors.colorSchemes.dracula;

  programs.home-manager.enable = true;
  fonts.fontconfig.enable = true;

  home = {
    inherit username;
    homeDirectory = "/home/${username}";
    stateVersion = "20.09";
  };
}
