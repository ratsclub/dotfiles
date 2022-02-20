{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # fonts
    jetbrains-mono
    overpass

    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    noto-fonts-extra

    # chat
    discord
    signal-desktop
    tdesktop

    # misc
    bitwarden
  ];
}
