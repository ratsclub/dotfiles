{ pkgs, config, super, lib, ... }:

let
  colorscheme = config.colorscheme.colors;

  kitty = "${pkgs.kitty}/bin/kitty";

  grimshot = "${pkgs.sway-contrib.grimshot}/bin/grimshot";
  mako = "${pkgs.mako}/bin/mako";
  pactl = "${pkgs.pulseaudio}/bin/pactl";
  pavucontrol = "${pkgs.pavucontrol}/bin/pavucontrol";

  swaybg = "${pkgs.swaybg}/bin/swaybg";
  swayfader = "${pkgs.nur.repos.misterio.swayfader}/bin/swayfader";
  swayidle = "${pkgs.swayidle}/bin/swayidle";
  swaylock = "${pkgs.swaylock-effects}/bin/swaylock";
  waybar = "${pkgs.waybar}/bin/waybar";
  wofi = "${pkgs.wofi}/bin/wofi";

  wallpaper = "${pkgs.nixos-artwork.wallpapers.nineish-dark-gray.gnomeFilePath}";
in
{
  wayland.windowManager.sway = {
    enable = super.device.type == "graphical";
    systemdIntegration = true;
    wrapperFeatures.gtk = true;
    config = rec {
      bars = [ ];
      terminal = "${kitty}";
      workspaceAutoBackAndForth = true;

      modifier = "Mod4";
      menu =
        "${wofi} -D run-always_parse_args=true -k /dev/null -i -e -S run -t ${terminal}";
      colors = {
        focused = {
          border = "${colorscheme.base0C}";
          background = "${colorscheme.base00}";
          text = "${colorscheme.base05}";
          indicator = "${colorscheme.base09}";
          childBorder = "${colorscheme.base0C}";
        };
        focusedInactive = {
          border = "${colorscheme.base03}";
          background = "${colorscheme.base00}";
          text = "${colorscheme.base04}";
          indicator = "${colorscheme.base03}";
          childBorder = "${colorscheme.base03}";
        };
        unfocused = {
          border = "${colorscheme.base02}";
          background = "${colorscheme.base00}";
          text = "${colorscheme.base03}";
          indicator = "${colorscheme.base02}";
          childBorder = "${colorscheme.base02}";
        };
        urgent = {
          border = "${colorscheme.base09}";
          background = "${colorscheme.base00}";
          text = "${colorscheme.base03}";
          indicator = "${colorscheme.base09}";
          childBorder = "${colorscheme.base09}";
        };
      };
      output = {
        "HDMI-A-1" = {
          scale = "1.50";
        };
      };
      startup = [
        # Set wallpaper
        { command = "${swaybg} -i ${wallpaper} -m fill"; }
        # Start idle daemon
        { command = "${swayidle} -w"; }
        # Start waybar
        { command = "${waybar}"; }
        # Start mako
        { command = "${mako}"; }
      ];
      keybindings = lib.mkOptionDefault {
        # Screenshot
        "Print" = "exec ${grimshot} --notify copy screen";
        "Control+Print" = "exec ${grimshot} --notify copy output";

        "Control+Shift+Print" = "exec ${grimshot} --notify copy area";
        "${modifier}+Print" = "exec ${grimshot} --notify copy active";
        "Control+${modifier}+Print" = "exec ${grimshot} --notify copy window";

        # Lockscreen
        "${modifier}+Control+l" = "exec ${swaylock} --screenshots";

        # Volume
        "XF86AudioRaiseVolume" =
          "exec ${pactl} set-sink-volume @DEFAULT_SINK@ +5%";
        "XF86AudioLowerVolume" =
          "exec ${pactl} set-sink-volume @DEFAULT_SINK@ -5%";
        "XF86AudioMute" = "exec ${pactl} set-sink-mute @DEFAULT_SINK@ toggle";
        "Shift+XF86AudioMute" =
          "exec ${pactl} set-source-mute @DEFAULT_SOURCE@ toggle";
        "XF86AudioMicMute" =
          "exec ${pactl} set-source-mute @DEFAULT_SOURCE@ toggle";
      };
    };
  };

  services.swayidle = {
    enable = super.device.type == "graphical";
    events = [
      { event = "before-sleep"; command = "swaylock"; }
      { event = "lock"; command = "lock"; }
    ];
  };

  programs.waybar = {
    enable = super.device.type == "graphical";
    settings = [
      {
        layer = "bottom";
        height = 5;
        position = "bottom";
        modules-left = [
          "sway/workspaces"
          "sway/mode"
        ];
        modules-center = [ "sway/window" ];
        modules-right = [
          "battery"
          "custom/separator"
          "pulseaudio"
          "custom/separator"
          "cpu"
          "custom/separator"
          "memory"
          "clock"
          "tray"
        ];
        modules = {
          "custom/separator" = {
            format = "|";
            interval = "once";
            tooltip = false;
          };
          battery = {
            bat = "BAT0";
            interval = 40;
            format = "BAT {capacity}%";
            format-charging = "CHR {capacity}%";
          };
          clock = {
            format = "{:%d/%m %H:%M}";
            tooltip-format = ''
              <big>{:%Y %B}</big>
              <tt><small>{calendar}</small></tt>
            '';
          };
          cpu = {
            format = "CPU {usage}% - {avg_frequency}Ghz";
            interval = 1;
            tooltip = false;
          };
          disk = {
            format = "DISK {used}/{total}";
            tooltip = false;
          };
          memory = { format = "RAM {used:0.1f}G/{total:0.1f}G "; };
          pulseaudio = {
            format = "VOL {volume}%";
            format-muted = "MUTE";
            on-click = "${pavucontrol}";
          };
        };
      }
    ];
    style = ''
      * {
        border: none;
        border-radius: 0;
        font-family: Jetbrains Mono;
        font-size: 12px;
        margin: 0px 0;
        padding: 0 3px;
      }

      window#waybar {
        color: #${colorscheme.base05};
        background-color: #${colorscheme.base00};
        border-bottom: 2px solid #${colorscheme.base0C};
      }
      #workspaces button {
        margin: 0;
      }
      #workspaces button.visible,
      #workspaces button.focused {
        background-color: #${colorscheme.base01};
        color: #${colorscheme.base04};
      }
      #workspaces button.focused {
        color: #${colorscheme.base0A};
      }
      #pulseaudio {
        padding: 0 8px;
      }
      #clock {
        background-color: #${colorscheme.base0C};
        color: #${colorscheme.base00};
        margin: 0;
        padding: 0 12px;
      }
    '';
  };

  xdg.configFile."swaylock/config".text = ''
    effect-blur=20x3
    fade-in=0.1
    font=Fira Sans
    font-size=15
    line-uses-inside
    disable-caps-lock-text
    indicator-caps-lock
    indicator-radius=40
    indicator-idle-visible
    indicator-y-position=1000
    ring-color=#${colorscheme.base02}
    inside-wrong-color=#${colorscheme.base08}
    ring-wrong-color=#${colorscheme.base08}
    key-hl-color=#${colorscheme.base0B}
    bs-hl-color=#${colorscheme.base08}
    ring-ver-color=#${colorscheme.base09}
    inside-ver-color=#${colorscheme.base09}
    inside-color=#${colorscheme.base01}
    text-color=#${colorscheme.base07}
    text-clear-color=#${colorscheme.base01}
    text-ver-color=#${colorscheme.base01}
    text-wrong-color=#${colorscheme.base01}
    text-caps-lock-color=#${colorscheme.base07}
    inside-clear-color=#${colorscheme.base0C}
    ring-clear-color=#${colorscheme.base0C}
    inside-caps-lock-color=#${colorscheme.base09}
    ring-caps-lock-color=#${colorscheme.base02}
    separator-color=#${colorscheme.base02}
  '';

  xdg.configFile."wofi/style.css".text = ''
    window {
      margin: 0px;
      border: 1px solid #${colorscheme.base0C};
      background-color: #${colorscheme.base00};
    }

    #input {
      margin: 5px;
      border: none;
      color: #${colorscheme.base05};
      background-color: #${colorscheme.base02};
    }

    #inner-box {
      margin: 5px;
      border: none;
      background-color: #${colorscheme.base00};
    }

    #outer-box {
      margin: 5px;
      border: none;
      background-color: #${colorscheme.base00};
    }

    #scroll {
      margin: 0px;
      border: none;
    }

    #text {
      margin: 5px;
      border: none;
      color: #${colorscheme.base05};
    }

    #entry:selected {
      background-color: #${colorscheme.base02};
    }
  '';

  home.packages = with pkgs; [
    wl-clipboard

    # notify-send
    libnotify
  ];
}


