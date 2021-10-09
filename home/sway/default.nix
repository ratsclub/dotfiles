{ super, inputs, lib, config, pkgs, ... }:

let
  colorscheme = config.colorscheme;
in
{
  wayland.windowManager.sway = {
    enable = super.device.type == "graphical";
    config = {
      terminal = "${pkgs.kitty}/bin/kitty";
      modifier = "Mod4";
      colors = {
        focused = {
          border = "${colorscheme.colors.base0C}";
          childBorder = "${colorscheme.colors.base0C}";
          indicator = "${colorscheme.colors.base09}";
          background = "${colorscheme.colors.base00}";
          text = "${colorscheme.colors.base05}";
        };
        focusedInactive = {
          border = "${colorscheme.colors.base03}";
          childBorder = "${colorscheme.colors.base03}";
          indicator = "${colorscheme.colors.base03}";
          background = "${colorscheme.colors.base00}";
          text = "${colorscheme.colors.base04}";
        };
        unfocused = {
          border = "${colorscheme.colors.base02}";
          childBorder = "${colorscheme.colors.base02}";
          indicator = "${colorscheme.colors.base02}";
          background = "${colorscheme.colors.base00}";
          text = "${colorscheme.colors.base03}";
        };
        urgent = {
          border = "${colorscheme.colors.base09}";
          childBorder = "${colorscheme.colors.base09}";
          indicator = "${colorscheme.colors.base09}";
          background = "${colorscheme.colors.base00}";
          text = "${colorscheme.colors.base03}";
        };
      };
      bars = [
        {
          mode = "dock";
          hiddenState = "hide";
          position = "bottom";
          workspaceButtons = true;
          workspaceNumbers = true;
          statusCommand = "${pkgs.i3status}/bin/i3status";
          fonts = {
            names = [ "JetBrains Mono" ];
            size = 8.0;
          };
          trayOutput = "primary";
          colors = {
            background = "${colorscheme.colors.base00}";
            statusline = "${colorscheme.colors.base05}";
            separator = "${colorscheme.colors.base03}";
            focusedWorkspace = {
              background = "${colorscheme.colors.base0C}";
              border = "${colorscheme.colors.base09}";
              text = "${colorscheme.colors.base05}";
            };
            activeWorkspace = {
              border = "${colorscheme.colors.base0C}";
              background = "${colorscheme.colors.base00}";
              text = "#ffffff";
            };
            inactiveWorkspace = {
              border = "${colorscheme.colors.base03}";
              background = "${colorscheme.colors.base00}";
              text = "${colorscheme.colors.base04}";
            };
            urgentWorkspace = {
              background = "${colorscheme.colors.base00}";
              border = "${colorscheme.colors.base09}";
              text = "${colorscheme.colors.base03}";
            };
            bindingMode = {
              border = "#2f343a";
              background = "#900000";
              text = "#ffffff";
            };
          };
        }
      ];
    };
  };

}
