{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    jetbrains.rider
    jetbrains.pycharm-professional
    signal-desktop
  ];
}
