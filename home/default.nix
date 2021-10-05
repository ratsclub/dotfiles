{ username, ... }:

{
  imports = [
    ./cli
    ./chromium
    ./firefox
    ./gui
    ./neovim
    ./vscodium
  ];

  programs.home-manager.enable = true;
  fonts.fontconfig.enable = true;

  home = {
    inherit username;
    homeDirectory = "/home/${username}";
    stateVersion = "20.09";
  };
}
