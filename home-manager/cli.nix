# Use this file to manage systems without a graphical interface
{
  imports = [
    ./pkgs/bash
    ./pkgs/cli-tools
    ./pkgs/direnv
    ./pkgs/emacs
    ./pkgs/git
    ./pkgs/neovim
    ./pkgs/newsboat
  ];

  programs.home-manager.enable = true;
  fonts.fontconfig.enable = true;

  home.username = "victor";
  home.homeDirectory = "/home/victor";

  home.stateVersion = "20.09";
}
