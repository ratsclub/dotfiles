{ username, ... }:

{
  imports = [
    ./cli
    ./gui
    ./vscodium
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
