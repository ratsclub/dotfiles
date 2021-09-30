{ username, ... }:

{
  imports = [
    ./cli
    ./gui
    ./vscodium
    ./firefox
    ./neovim
  ];

  programs.home-manager.enable = true;
  fonts.fontconfig.enable = true;

  home = {
    inherit username;
    homeDirectory = "/home/${username}";
    stateVersion = "20.09";
  };
}
