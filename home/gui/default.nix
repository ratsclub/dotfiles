{ pkgs, ... }:

{
  home.packages = with pkgs; [
    discord
    tdesktop
    bitwarden
    obsidian
  ];
}
