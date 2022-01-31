{ super, pkgs, ... }:

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
    element-desktop
    signal-desktop
    tdesktop

    # misc
    bitwarden
    obsidian
  ];
}
