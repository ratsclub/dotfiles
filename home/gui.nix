{ super, pkgs, ... }:

{
  home.packages =
    let
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

      # ebooks
      foliate

      # misc
      bitwarden
      obsidian
    ] ++ jetbrains-ides
    else [ ];
}
