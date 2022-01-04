{ pkgs, config, super, ... }:

let
  colors = config.colorscheme.colors;
in
{
  programs.mako = {
    enable = super.device.type == "graphical";
    font = "Jetbrains Mono";
    padding = "10,20";
    anchor = "top-center";
    width = 400;
    height = 150;
    borderSize = 2;
    defaultTimeout = 12000;
    backgroundColor = "#${colors.base00}dd";
    borderColor = "#${colors.base03}dd";
    textColor = "#${colors.base05}dd";
  };
}
