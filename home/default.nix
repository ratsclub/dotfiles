{ username, inputs, ... }:

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

    inputs.nix-colors.homeManagerModule
  ];

  colorscheme = inputs.nix-colors.colorSchemes.catppuccin;

  programs.home-manager.enable = true;
  fonts.fontconfig.enable = true;

  home = {
    inherit username;
    homeDirectory = "/home/${username}";
    stateVersion = "20.09";
  };
}
