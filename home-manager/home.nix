{ pkgs, inputs, system, ... }:

let
  unstable = import inputs.unstable {
    inherit system;
  };
in
{
  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;
  home-manager.users.ratsclub = {
    programs.home-manager.enable = true;

    fonts.fontconfig.enable = true;

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
          "workbench.iconTheme" = "file-icons";

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
          file-icons.file-icons

          # Nix
          jnoortheen.nix-ide

          # Go
          golang.Go

          # Python
          ms-python.python

          # Rust
          matklad.rust-analyzer

          # Markdown
          foam.foam-vscode
          svsool.markdown-memo
          yzhang.markdown-all-in-one

          # Misc
          eamodio.gitlens
          esbenp.prettier-vscode
        ] ++ unstable.vscode-utils.extensionsFromVscodeMarketplace [
          {
            name = "ng-template";
            publisher = "Angular";
            version = "12.1.1";
            sha256 = "sha256-KklXgLj1AuHNqlyiQi89ruPRfIuGHU84pqDmRfX0c1Q=";
          }
        ];
      };
    };

    # neovim
    programs = {
      neovim = {
        enable = true;

        viAlias = true;
        vimAlias = true;

        withNodeJs = true;
        withPython3 = true;
        withRuby = true;

        extraConfig = builtins.readFile ../config/nvim/config.vim;

        plugins = with pkgs.vimPlugins; [
          coc-nvim
          coc-tsserver
          fzf-vim
          vim-nix
        ];
      };
    };

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

    home.packages = with pkgs; [
      # book readers
      calibre
      foliate

      # chats
      discord
      tdesktop

      # cli
      aria2
      fd
      gnumake
      lazygit
      ripgrep
      nixpkgs-fmt
      rnix-lsp

      # fonts
      ibm-plex

      # gui
      bitwarden
      obsidian
    ];

    home.stateVersion = "20.09";
  };

}
