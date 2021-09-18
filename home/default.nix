{ username, homeDirectory, ... }:

{
  imports = [
    ./cli
    ./gui
    ./vscode
  ];

  programs.home-manager.enable = true;
  fonts.fontconfig.enable = true;

  home = {
    inherit username homeDirectory;
    stateVersion = "20.09";
  };
}
