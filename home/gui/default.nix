{ pkgs, ... }:

{
  home.packages = with pkgs; [
    discord
    tdesktop
    
    bitwarden
    firefox
    obsidian

    jetbrains.idea-community
    jetbrains.pycharm-community
  ];
}
