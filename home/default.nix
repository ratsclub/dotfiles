{ username, homeDirectory, ... }:

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
    inherit username homeDirectory;
    stateVersion = "20.09";
  };
}
