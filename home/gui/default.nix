{ super, pkgs, ... }:

{
  home.packages =
    if super.device.type == "graphical"
    then with pkgs; [
      # chat
      discord
      tdesktop

      # editors
      jetbrains.idea-community
      jetbrains.pycharm-community

      # ebooks
      foliate

      # misc
      bitwarden
      firefox
      obsidian
    ]
    else [ ];
}
