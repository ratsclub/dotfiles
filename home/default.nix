{ username, inputs, ... }:

{
  imports = [
    ./chromium.nix
    ./cli
    ./firefox.nix
    ./gui.nix
    ./kitty.nix
    ./neovim.nix
    ./vscodium.nix
    inputs.nix-colors.homeManagerModule
  ];

  colorscheme = inputs.nix-colors.colorSchemes.gruvbox-dark-hard;

  programs.home-manager.enable = true;
  fonts.fontconfig.enable = true;

  home = {
    inherit username;
    homeDirectory = "/home";
    stateVersion = "20.09";
  };
}
