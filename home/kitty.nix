{ pkgs, inputs, config, ... }:

let colors = config.colorscheme.colors;
in
{
  programs.kitty = {
    enable = true;
    font = {
      package = pkgs.jetbrains-mono;
      name = "JetBrains Mono";
      size = 10;
    };

    settings = {
      disable_ligatures = "always";
      window_padding_width = 2;
      foreground = "#${colors.base05}";
      background = "#${colors.base00}";
      selection_background = "#${colors.base05}";
      selection_foreground = "#${colors.base00}";
      url_color = "#${colors.base04}";
      cursor = "#${colors.base05}";
      active_border_color = "#${colors.base03}";
      inactive_border_color = "#${colors.base01}";
      active_tab_background = "#${colors.base00}";
      active_tab_foreground = "#${colors.base05}";
      inactive_tab_background = "#${colors.base01}";
      inactive_tab_foreground = "#${colors.base04}";
      tab_bar_background = "#${colors.base01}";
      color0 = "#${colors.base00}";
      color1 = "#${colors.base08}";
      color2 = "#${colors.base0B}";
      color3 = "#${colors.base0A}";
      color4 = "#${colors.base03}";
      color5 = "#${colors.base0E}";
      color6 = "#${colors.base0C}";
      color7 = "#${colors.base05}";
      color8 = "#${colors.base03}";
      color9 = "#${colors.base08}";
      color10 = "#${colors.base0B}";
      color11 = "#${colors.base0A}";
      color12 = "#${colors.base03}";
      color13 = "#${colors.base0E}";
      color14 = "#${colors.base0C}";
      color15 = "#${colors.base07}";
      color16 = "#${colors.base09}";
      color17 = "#${colors.base0F}";
      color18 = "#${colors.base01}";
      color19 = "#${colors.base02}";
      color20 = "#${colors.base04}";
      color21 = "#${colors.base06}";
    };
  };
}
