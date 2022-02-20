{ config, pkgs, inputs, ... }:

let
  inherit (pkgs) nixos-artwork;
  wallpaper = nixos-artwork.wallpapers.nineish-dark-gray.gnomeFilePath;
in
{
  dconf.settings = {
    "org/gnome/desktop/wm/keybindings" = {
      "switch-windows" = [ "<Alt>Tab" ];
    };
    "org/gnome/desktop/wm/preferences" = {
      "resize-with-right-button" = true;
    };
    "org/gnome/desktop/background" = {
      "picture-uri" = wallpaper;
    };
    "org/gnome/desktop/interface" = {
      "enable-hot-corners" = false;
      "show-battery-percentage" = true;
    };
    "org/gnome/mutter" = {
      "edge-tiling" = true;
    };
  };
}
