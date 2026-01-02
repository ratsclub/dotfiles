{ pkgs, ... }:

let
  inherit (pkgs)
    millet
    nixfmt-tree
    nixd
    rust-analyzer
    ;
in
{
  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;
    profiles.default = {
      userSettings = {
        "update.mode" = "none";

        "editor.formatOnSave" = false;
        "editor.linkedEditing" = true;
        "editor.rulers" = [
          80
          120
        ];

        # excluded files
        "files.exclude" = {
          # removes these from the search
          "**/.direnv" = true;
          "**/.devenv" = true;
        };

        "terminal.integrated.tabs.enabled" = true;
        "terminal.integrated.defaultProfile.osx" = "zsh";
        "terminal.integrated.profiles.osx" = {
          "zsh" = {
            "path" = "zsh";
            "args" = [ ];
          };
        };

        "workbench.tree.indent" = 15;
        "workbench.colorTheme" = "Default Dark Modern";

        "window.titleBarStyle" = "custom";
        "window.zoomLevel" = 0;

        # F#
        "FSharp.inlayHints.enabled" = false;
        "FSharp.inlayHints.typeAnnotations" = false;
        "FSharp.inlayHints.parameterNames" = false;
        "FSharp.addFsiWatcher" = true;
        "FSharp.FSIExtraInteractiveParameters" = [ "--readline" ];
        "FSharp.FSIExtraSharedParameters" = [ "--readline" ];
        "FSharp.saveOnSendLastSelection" = false;

        # HTML
        "[html]" = {
          "editor.defaultFormatter" = "esbenp.prettier-vscode";
        };

        # Nix
        "nix" = {
          "enableLanguageServer" = true;
          "serverPath" = "${nixd}/bin/nixd";
          "formatterPath" = [
            "${nixfmt-tree}/bin/treefmt"
            "--stdin"
            "{file}"
          ];
        };
        "[nix]" = {
          "editor.formatOnSave" = true;
          "editor.insertSpaces" = true;
          "editor.tabSize" = 2;
        };

        # Python
        "[python]" = {
          "editor.formatOnSave" = true;
          "editor.defaultFormatter" = "charliermarsh.ruff";
        };

        # Rust
        "rust-analyzer.server.path" = "${rust-analyzer}/bin/rust-analyzer";

        # Standard ML
        "[sml]" = {
          "editor.defaultFormatter" = "azdavis.millet";
          "editor.formatOnSave" = true;
        };
        "millet.format.engine" = "smlfmt";
        "millet.server.path" = "${millet}/bin/millet-ls";
      };

      extensions = with pkgs.vscode-extensions; [
        # Nix
        jnoortheen.nix-ide

        # Go
        golang.go

        # Rust
        rust-lang.rust-analyzer

        # Standard ML
        azdavis.millet

        # Markdown
        yzhang.markdown-all-in-one

        # Misc
        editorconfig.editorconfig
        esbenp.prettier-vscode
        gruntfuggly.todo-tree
        mkhl.direnv
      ];
    };
  };

  programs.vscode.profiles.default.userSettings.editor.fontFamily = "Jetbrains Mono";
  home.packages = with pkgs; [ jetbrains-mono ];
}
