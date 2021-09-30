{ pkgs, ... }:

{
  home.packages = with pkgs; [
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
  ];
}
