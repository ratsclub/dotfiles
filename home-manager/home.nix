{ pkgs, inputs, system, ... }:

let
  unstable = import inputs.unstable {
    inherit system;
    config = {
      allowUnfree = true;
    };
  };
  agenix = inputs.agenix.defaultPackage."${system}";
  wallpaper = "${pkgs.nixos-artwork.wallpapers.nineish-dark-gray}/share/wallpapers/nineish-dark-gray-2020-07-02/contents/images/nix-wallpaper-nineish-dark-gray.png";
in
{
  programs.home-manager.enable = true;
  fonts.fontconfig.enable = true;

  home.username = "ratsclub";
  home.homeDirectory = "/home/ratsclub";

  # dconf
  dconf = {
    enable = true;
    settings = {
      "org/gnome/desktop/peripherals/mouse" = {
        "left-handed" = true;
      };

      "org/gnome/desktop/background" = {
        "picture-uri" = wallpaper;
      };

      "org/gnome/nautilus/list-view" = {
        "use-tree-view" = true;
        "default-zoom-level" = "small";
      };
    };
  };

  # kitty
  programs = {
    kitty = {
      enable = true;
      font.package = pkgs.ibm-plex;
      font.name = "IBM Plex Mono";
      font.size = 9;

      settings = {
        window_padding_width = 0;
        enable_audio_bell = false;
      };
    };
  };

  # command-line
  programs = {
    bat.enable = true;
    exa.enable = true;
    jq.enable = true;

    direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
        enableFlakes = true;
      };
    };

    git = {
      enable = true;

      userName = "Victor Freire";
      userEmail = "victor@freire.dev.br";
      package = pkgs.gitFull;

      delta = {
        enable = true;
        options = {
          features = "side-by-side line-numbers decorations";
          syntax-theme = "Dracula";
          plus-style = ''syntax "#003800"'';
          minus-style = ''syntax "#3f0001"'';
          decorations = {
            commit-decoration-style = "bold yellow box ul";
            file-style = "bold yellow ul";
            file-decoration-style = "none";
            hunk-header-decoration-style = "cyan box ul";
          };
          delta = {
            navigate = true;
          };
          line-numbers = {
            line-numbers-left-style = "cyan";
            line-numbers-right-style = "cyan";
            line-numbers-minus-style = 124;
            line-numbers-plus-style = 28;
          };
        };
      };
    };

    bash = {
      enable = true;
      historySize = 10000;
      shellOptions = [
        # Append to history file rather than replacing it.
        "histappend"

        # check the window size after each command and, if
        # necessary, update the values of LINES and COLUMNS.
        "checkwinsize"

        # Extended globbing.
        "extglob"
        "globstar"

        # Warn if closing shell with running jobs.
        "checkjobs"
      ];

      historyControl = [
        "erasedups"
        "ignoredups"
        "ignorespace"
      ];

      shellAliases = {
        ls = "ls --color=auto";
        ll = "ls -l";
        la = "ls -A";
        lt = "ls --human-readable --size -1 -S --classify";
        l = "ls -CF";
        grep = "grep --color=auto";
        ".." = "cd ..";
        mknote = ">$(cat /dev/urandom | tr -dc 'a-z0-9' | fold -w 8 | head -n 1).md";
      };

      sessionVariables = {
        EDITOR = "vi";
      };

      profileExtra = builtins.readFile ../config/bash/.profile;
      bashrcExtra = builtins.readFile ../config/bash/.bashrc;
    };

    fzf = {
      enable = true;
      enableBashIntegration = true;
    };
  };

  home.file.".config/direnv/lib/use_flake.sh".text = ''
    use_flake() {
      watch_file flake.nix
      watch_file flake.lock
      eval "$(nix print-dev-env --profile "$(direnv_layout_dir)/flake-profile")"
    }
  '';

  # Visual Studio Code
  programs = {
    vscode = {
      enable = true;
      package = unstable.vscodium;
      userSettings = {
        # auto update tags when edited
        "editor.linkedEditing" = true;
        "editor.rulers" = [ 80 120 ];
        "editor.formatOnSave" = true;

        "workbench.colorTheme" = "GitHub Dark";
        "workbench.iconTheme" = "material-icon-theme";

        "window.titleBarStyle" = "custom";
        "window.zoomLevel" = 0;
        "terminal.integrated.tabs.enabled" = true;
        "[html]" = {
          "editor.defaultFormatter" = "esbenp.prettier-vscode";
        };
        "nix.enableLanguageServer" = true;
        "editor.fontFamily" = "IBM Plex Mono";
      };
      extensions = with unstable.vscode-extensions; [
        # Theme
        github.github-vscode-theme

        # Icons
        pkief.material-icon-theme

        # Angular
        angular.ng-template

        # Nix
        jnoortheen.nix-ide

        # Go
        golang.go

        # Python
        ms-python.python
        ms-toolsai.jupyter

        # Rust
        matklad.rust-analyzer

        # Markdown
        foam.foam-vscode
        svsool.markdown-memo
        yzhang.markdown-all-in-one

        # Misc
        eamodio.gitlens
        esbenp.prettier-vscode
      ];
    };
  };

  # neovim
  programs = {
    neovim = {
      enable = true;

      viAlias = true;
      vimAlias = true;

      extraConfig = builtins.readFile ../config/nvim/config.vim;
    };
  };

  xsession.windowManager.i3 = {
    enable = true;
    config = {
      modifier = "Mod4";
      bars = [];

      startup = [
        {
          command = "${pkgs.feh}/bin/feh --bg-scale '${wallpaper}'";
        }
        {
          command = "systemctl --user restart polybar.service";
        }
      ];

      keybindings = pkgs.lib.mkOptionDefault {
        "XF86AudioLowerVolume" = "exec amixer set Master 5%-";
        "XF86AudioRaiseVolume" = "exec amixer set Master 5%+";
        "XF86AudioMute" = "exec amixer set Master toggle";
      };
    };
  };

  services.polybar = {
    enable = true;
    package = pkgs.polybarFull;
    script = ''
      polybar bottom &
    '';

    settings = {
      "bar/bottom" = {
        bottom = true;
        width = "100%";
        height = "3%";
        radius = 0;
        modules-left = "i3";
        modules-center = "date";
        modules-right = "battery";
        tray-position = "right";
        wm-restack = "i3";
      };

      "module/date" = {
        type = "internal/date";
        internal = 5;
        date = "%d.%m.%y";
        time = "%H:%M";
        label = "%time%  %date%";
      };

      "module/battery" = {
        type = "internal/battery";

        battery = "BAT0";
        adapter = "AC";

        poll-interval = 5;
      };

      "module/i3" = {
        type = "internal/i3";

        pin-workspaces = true;
        strip-wsnumbers = true;
        index-sort = true;

        fuzzy-match = true;

        ws-icon-0 = "1;♚";
        poll-interval = 5;
        ws-icon-1 = "2;♛";
        ws-icon-2 = "3;♜";
        ws-icon-3 = "4;♝";
        ws-icon-4 = "5;♞";
        ws-icon-default = "♟";

        # label-dimmed-underline = ${root.background}

        format = "<label-state> <label-mode>";

        label-mode = "%mode%";
        label-mode-padding = 0;
        label-mode-background = "#e60053";

        label-focused = "%index%";
        label-focused-foreground = "#ffffff";
        label-focused-background = "#3f3f3f";
        label-focused-underline = "#fba922";
        label-focused-padding = 1;

        label-unfocused = "%index%";
        label-unfocused-padding = 1;

        label-visible = "%index%";
        label-visible-underline = "#555555";
        label-visible-padding = 1;

        label-urgent = "%index%";
        label-urgent-foreground = "#000000";
        label-urgent-background = "#bd2c40";
        label-urgent-padding = 1;

        label-separator = "|";
        label-separator-padding = 0;
        label-separator-foreground = "#ffb52a";
      };
    };
  };

  services.network-manager-applet.enable = true;

  # firefox
  programs = {
    firefox = {
      enable = true;
      profiles.ratsclub = {
        settings = {
          # https://wiki.archlinux.org/title/Firefox#Hardware_video_acceleration
          "gfx.webrender.all" = true;
          "browser.quitShortcut.disabled" = true;
          "media.ffmpeg.vaapi.enabled" = true;
          "media.ffvpx.enabled" = true;
          "media.navigator.mediadatadecoder_vpx_enabled" = true;
        };
      };
      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        privacy-badger
        ublock-origin
        bitwarden
      ];
    };
  };

  programs.newsboat = {
    enable = true;
    urls = [
      {
        title = "Lobsters";
        url = "https://lobste.rs/rss";
        tags = [ "tech" ];
      }
      {
        title = "Drew DeVault's blog";
        url = "https://drewdevault.com/blog/index.xml";
        tags = [ "tech" ];
      }
      {
        title = "Andrea Della Corte";
        url = "https://www.dellacorte.me/feed/newsletters.xml";
        tags = [ "personal" ];
      }
      {
        title = "Hundred Rabbits";
        url = "https://100r.co/links/rss.xml";
        tags = [ "personal" ];
      }
    ];

    extraConfig = builtins.readFile ../config/newsboat/config;
  };

  home.packages = with pkgs; [
    # book readers
    calibre
    foliate

    # chats
    discord
    tdesktop

    # cli
    agenix
    aria2
    fd
    gnumake
    lazygit
    nixpkgs-fmt
    ripgrep
    rnix-lsp

    # fonts
    ibm-plex

    # gui
    bitwarden
    obsidian
  ];

  home.stateVersion = "20.09";
}
