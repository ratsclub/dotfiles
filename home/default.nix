{ username, inputs, ... }:

{
  imports = [
    ./chromium
    ./cli
    ./firefox
    ./gui
    ./neovim
    ./sway
    ./vscodium
    ./kitty
    inputs.nix-colors.homeManagerModule
  ];

  colorscheme = inputs.nix-colors.colorSchemes.pinky;

  programs.home-manager.enable = true;
  fonts.fontconfig.enable = true;

  home = {
    inherit username;
    homeDirectory = "/home/${username}";
    stateVersion = "20.09";
  };
}
