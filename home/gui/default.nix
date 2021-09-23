{ pkgs, ... }:

{
  home.packages = with pkgs; [
    bitwarden
    discord
    firefox
    obsidian
    tdesktop
  ];
}
