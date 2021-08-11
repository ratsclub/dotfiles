# Use this file to manage systems without a graphical interface
{
  imports = [
    ./pkgs/bash
    ./pkgs/cli-tools
    ./pkgs/direnv
    ./pkgs/git
    ./pkgs/neovim
    ./pkgs/newsboat
  ];

  programs.home-manager.enable = true;
  fonts.fontconfig.enable = true;

  home.username = "ratsclub";
  home.homeDirectory = "/home/ratsclub";

  home.stateVersion = "20.09";
}
