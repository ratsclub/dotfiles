{ super, pkgs, ... }:

{
  home.packages =
    let
      fonts = with pkgs; [
        jetbrains-mono
        overpass

        noto-fonts
        noto-fonts-extra
        noto-fonts-emoji
        noto-fonts-cjk
      ];

      jetbrains-ides = with pkgs.jetbrains; [
        datagrip
        goland
        idea-ultimate
        pycharm-professional
      ];
    in
    if super.device.type == "graphical"
    then with pkgs; [
      # chat
      discord
      element-desktop
      tdesktop
      slack

      # ebooks
      foliate

      # misc
      bitwarden
      obsidian

      # bluetooth
      blueberry
    ]
    ++ jetbrains-ides
    ++ fonts
    else [ ];
}
