{ pkgs, ... }:

{
  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;
  home-manager.users.ratsclub = {
    programs.home-manager.enable = true;

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
        package = pkgs.vscodium;
        userSettings = {
          "editor.rulers" = [ 80 120 ];
          "workbench.colorTheme" = "GitHub Dark";
          "window.titleBarStyle" = "custom";
          "workbench.iconTheme" = "file-icons";
          "window.zoomLevel" = 0;
        };
        extensions = with pkgs.vscode-extensions; [
          # Theme
          github.github-vscode-theme

          # CSharp
          ms-dotnettools.csharp

          # Icons
          file-icons.file-icons

          # Nix
          jnoortheen.nix-ide

          # Go
          golang.Go

          # Rust
          matklad.rust-analyzer

          # Markdown
          yzhang.markdown-all-in-one
        ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
          {
            name = "Ionide-fsharp";
            publisher = "Ionide";
            version = "5.5.6";
            sha256 = "CJf535sRmSnhJ+FZY+QUmeMbtIwqr1pFcZBjPTp4bM0=";
          }
          {
            name = "foam-vscode";
            publisher = "foam";
            version = "0.13.7";
            sha256 = "Y2pcd4iXPiuhJdD/9d+tbTJN18O4+kRMqUdOtbx8xy8=";
          }
          {
            name = "markdown-memo";
            publisher = "svsool";
            version = "0.3.8";
            sha256 = "eFiCCXxrOnXwJK1AOMfIDsPGsFG3ArLD1X/uAEH5lRY=";
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
          coc-go
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
      fd
      nixpkgs-fmt
      ripgrep

      bitwarden
      discord
      tdesktop
    ];

    home.stateVersion = "20.09";
  };


}



